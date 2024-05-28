{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf
  ( module OptEnvConf,
    module OptEnvConf.Parser,
    module Control.Applicative,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as JSON
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity hiding (Validation)
import GHC.Generics (Generic)
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..))
import qualified OptEnvConf.ArgMap as AM
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EM
import OptEnvConf.Parser
import System.Environment (getArgs, getEnvironment)
import System.Exit
import Text.Colour
import Text.Colour.Layout

data ArgParser a = ArgParser
  { argParserParse :: !(String -> Either String a),
    argParserShort :: ![Char], -- TODO use dashed?
    argParserLong :: ![String]
  }

data EnvParser a = EnvParser
  { envParserParse :: !(String -> Either String a),
    envParserVar :: !String
  }

envVar :: String -> Parser (Maybe String)
envVar = ParserEnvVar

strArg :: Parser String
strArg = ParserArg

strArgs :: Parser [String]
strArgs = ParserArgs

strOpt :: String -> Parser (Maybe String)
strOpt = ParserOpt . NE.singleton . DashedLong . NE.fromList -- TODO unsafe

argLeftovers :: Parser [String]
argLeftovers = ParserArgLeftovers

-- Arguments _and_ leftovers
allArgs :: Parser [String]
allArgs = (++) <$> strArgs <*> argLeftovers

confVar :: String -> Parser (Maybe String)
confVar = ParserConfig

optionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
optionalFirst = ParserOptionalFirst

requiredFirst :: [Parser (Maybe a)] -> Parser a
requiredFirst = ParserRequiredFirst

parserDocs :: Parser a -> AnyDocs AnyDoc
parserDocs = go
  where
    go :: Parser a -> AnyDocs AnyDoc
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> AnyDocsSingle []
      ParserAp pf pa -> AnyDocsAnd [go pf, go pa]
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserOptionalFirst ps -> AnyDocsOr $ map go ps
      ParserRequiredFirst ps -> AnyDocsOr $ map go ps
      ParserArg -> AnyDocsSingle [AnyDocOpt $ OptDocArg Nothing]
      ParserArgs -> AnyDocsSingle [AnyDocOpt $ OptDocArgs Nothing]
      ParserOpt d -> AnyDocsSingle [AnyDocOpt $ OptDocOpt d Nothing]
      ParserArgLeftovers -> AnyDocsSingle [AnyDocOpt $ OptDocLeftovers Nothing]
      ParserEnvVar v ->
        AnyDocsSingle
          [ AnyDocEnv $
              EnvDoc
                { envDocVar = v,
                  envDocHelp = Nothing
                }
          ]
      ParserConfig _ -> AnyDocsSingle []

renderDocs :: AnyDocs AnyDoc -> [Chunk]
renderDocs =
  unlinesChunks
    . go
    . simplifyAnyDocs
  where
    go :: AnyDocs AnyDoc -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> concatMap renderAnyDoc vs

data AnyDoc
  = AnyDocOpt !OptDoc
  | AnyDocEnv !EnvDoc

renderAnyDoc :: AnyDoc -> [[Chunk]]
renderAnyDoc = \case
  AnyDocOpt d -> renderOptDocLong d
  AnyDocEnv d -> renderEnvDoc d

data AnyDocs a
  = AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle ![a]

instance Functor AnyDocs where
  fmap f = \case
    AnyDocsAnd as -> AnyDocsAnd $ fmap (fmap f) as
    AnyDocsOr as -> AnyDocsOr $ fmap (fmap f) as
    AnyDocsSingle as -> AnyDocsSingle $ fmap f as

instance Foldable AnyDocs where
  foldMap f = \case
    AnyDocsAnd as -> foldMap (foldMap f) as
    AnyDocsOr as -> foldMap (foldMap f) as
    AnyDocsSingle as -> foldMap f as

instance Traversable AnyDocs where
  traverse f = \case
    AnyDocsAnd as -> AnyDocsAnd <$> traverse (traverse f) as
    AnyDocsOr as -> AnyDocsOr <$> traverse (traverse f) as
    AnyDocsSingle as -> AnyDocsSingle <$> traverse f as

type OptDocs = AnyDocs OptDoc

data OptDoc
  = OptDocArg !(Maybe String)
  | OptDocArgs !(Maybe String)
  | OptDocOpt !(NonEmpty Dashed) !(Maybe String)
  | OptDocLeftovers !(Maybe String)

parserOptDocs :: Parser a -> OptDocs
parserOptDocs =
  fromMaybe (AnyDocsSingle [])
    . traverse
      ( \case
          AnyDocOpt d -> Just d
          _ -> Nothing
      )
    . parserDocs

renderCompleteOptDocs :: OptDocs -> [Chunk]
renderCompleteOptDocs optDocs =
  unlinesChunks
    [ renderShortOptDocs optDocs,
      renderLongOptDocs optDocs
    ]

renderShortOptDocs :: OptDocs -> [Chunk]
renderShortOptDocs = go . simplifyAnyDocs
  where
    go :: OptDocs -> [Chunk]
    go = \case
      AnyDocsAnd ds -> unwordsChunks $ map go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs ->
        unwordsChunks $
          map
            ( \case
                OptDocArg _ ->
                  [ "ARG"
                  ]
                OptDocArgs _ ->
                  [ "[ARG]"
                  ]
                OptDocOpt flags _ ->
                  [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed $ NE.toList flags
                  ]
                OptDocLeftovers _ ->
                  [ "LEFTOVERS"
                  ]
            )
            vs

unwordsChunks :: [[Chunk]] -> [Chunk]
unwordsChunks = intercalate [" "]

renderLongOptDocs :: OptDocs -> [Chunk]
renderLongOptDocs =
  layoutAsTable
    . go
    . simplifyAnyDocs
  where
    go :: OptDocs -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> concatMap renderOptDocLong vs

renderOptDocLong :: OptDoc -> [[Chunk]]
renderOptDocLong =
  (: []) . \case
    OptDocArg mDoc ->
      [ "ARG",
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]
    OptDocArgs mDoc ->
      [ "[ARG]",
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]
    OptDocOpt flags mDoc ->
      [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed $ NE.toList flags,
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]
    OptDocLeftovers mDoc ->
      [ "leftovers",
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]

type EnvDocs = AnyDocs EnvDoc

data EnvDoc = EnvDoc
  { envDocVar :: !String,
    envDocHelp :: !(Maybe String)
  }

parserEnvDocs :: Parser a -> EnvDocs
parserEnvDocs =
  fromMaybe (AnyDocsSingle [])
    . traverse
      ( \case
          AnyDocEnv d -> Just d
          _ -> Nothing
      )
    . parserDocs

renderEnvDocs :: EnvDocs -> Text
renderEnvDocs =
  renderChunksText With24BitColours
    . layoutAsTable
    . go
    . simplifyAnyDocs
  where
    go :: EnvDocs -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> concatMap renderEnvDoc vs

renderEnvDoc :: EnvDoc -> [[Chunk]]
renderEnvDoc EnvDoc {..} =
  [ [ chunk $ T.pack envDocVar,
      chunk . T.pack $ fromMaybe "undocumented" envDocHelp
    ]
  ]

simplifyAnyDocs :: AnyDocs a -> AnyDocs a
simplifyAnyDocs = go
  where
    go = \case
      AnyDocsAnd ds -> AnyDocsAnd $ concatMap goAnd ds
      AnyDocsOr ds -> AnyDocsOr $ concatMap goOr ds
      AnyDocsSingle vs -> AnyDocsSingle vs

    goAnd = \case
      AnyDocsAnd ds -> concatMap goAnd ds
      ds -> [ds]

    goOr = \case
      AnyDocsOr ds -> concatMap goOr ds
      ds -> [ds]

runParser :: Parser a -> IO a
runParser p = do
  args <- AM.parse <$> getArgs
  envVars <- EM.parse <$> getEnvironment
  let mConf = Nothing

  -- TODO do something with the leftovers
  case runParserPure p args envVars mConf of
    Left err -> die $ unlines $ map displayException $ NE.toList err
    Right (a, _) -> pure a

data ParseError
  = ParseErrorUnconsumed
  | ParseErrorRequired
  | ParseErrorMissingArgument
  | ParseErrorConfigParseError !String
  deriving (Show)

instance Exception ParseError where
  displayException = \case
    ParseErrorUnconsumed -> "Unconsumed arguments"
    ParseErrorRequired -> "Missing required setting" -- TODO show which ones
    ParseErrorMissingArgument -> "Missing required argument"
    ParseErrorConfigParseError err -> "Failed to parse configuration: " <> show err

runParserPure ::
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  Either (NonEmpty ParseError) (a, [String])
runParserPure p args envVars mConfig =
  validationToEither $ do
    let ppEnv = PPEnv {ppEnvEnv = envVars, ppEnvConf = mConfig}
    (result, unconsumedArgs) <- runStateT (runReaderT (go p) ppEnv) args
    when (AM.hasUnconsumed unconsumedArgs) $ Failure $ NE.singleton ParseErrorUnconsumed
    pure (result, AM.argMapLeftovers unconsumedArgs)
  where
    -- TODO maybe use validation instead of either
    -- TODO typed parseError
    go ::
      Parser a ->
      PP a
    go = \case
      ParserFmap f p' -> f <$> go p'
      ParserPure a -> pure a
      ParserAp ff fa -> go ff <*> go fa
      ParserAlt p1 p2 -> do
        s <- get
        env <- ask
        case runPP (go p1) s env of
          Right (a, s') -> do
            put s'
            pure a
          -- Note that args are not consumed if the alternative failed.
          Left _ -> go p2 -- TODO: Maybe collect the error?
      ParserOptionalFirst pss -> case pss of
        [] -> pure Nothing
        (p' : ps) -> do
          s <- get
          env <- ask
          case runPP (go p') s env of
            Left err -> ppErrors err -- Error if any fails, don't ignore it.
            Right (mA, s') -> case mA of
              Nothing -> go $ ParserOptionalFirst ps -- Don't record the state and continue to try to parse the next
              Just a -> do
                put s' -- Record the state
                pure (Just a)
      ParserRequiredFirst pss -> case pss of
        [] -> ppError ParseErrorRequired
        (p' : ps) -> do
          s <- get
          env <- ask
          case runPP (go p') s env of
            Left err -> ppErrors err -- Error if any fails, don't ignore it.
            Right (mA, s') -> case mA of
              Nothing -> go $ ParserRequiredFirst ps -- Don't record the state and continue to try to parse the next
              Just a -> do
                put s' -- Record the state of the parser that succeeded
                pure a
      ParserArg -> do
        mA <- ppArg
        case mA of
          Nothing -> ppError ParseErrorMissingArgument
          Just a -> pure a
      ParserArgs -> gets AM.argMapArgs -- TODO consume these args (?)
      ParserOpt _ -> undefined
      ParserArgLeftovers -> gets AM.argMapLeftovers
      ParserEnvVar v -> do
        es <- asks ppEnvEnv
        pure (EM.lookup v es)
      ParserConfig key -> do
        mConf <- asks ppEnvConf
        case mConf of
          Nothing -> pure Nothing
          Just conf -> case JSON.parseEither (.: Key.fromString key) conf of
            Left err -> ppError $ ParseErrorConfigParseError err
            Right v -> pure (Just v)

type PP a = ReaderT PPEnv (StateT ArgMap (Validation ParseError)) a

data PPEnv = PPEnv
  { ppEnvEnv :: !EnvMap,
    ppEnvConf :: !(Maybe JSON.Object)
  }

runPP ::
  PP a ->
  ArgMap ->
  PPEnv ->
  Either (NonEmpty ParseError) (a, ArgMap)
runPP p args envVars =
  validationToEither $ runStateT (runReaderT p envVars) args

ppArg :: PP (Maybe String)
ppArg = state AM.consumeArg

ppErrors :: NonEmpty ParseError -> PP a
ppErrors = lift . lift . Failure

ppError :: ParseError -> PP a
ppError = ppErrors . NE.singleton

data Validation e a
  = Failure !(NonEmpty e)
  | Success !a
  deriving (Generic, Show)

instance (Validity e, Validity a) => Validity (Validation e a)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Applicative (Validation e) where
  pure = Success
  Failure e1 <*> b = Failure $ case b of
    Failure e2 -> e1 `NE.append` e2
    Success _ -> e1
  Success _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success (f a)

instance Monad (Validation e) where
  return = pure
  Success a >>= f = f a
  Failure es >>= _ = Failure es

validationToEither :: Validation e a -> Either (NonEmpty e) a
validationToEither = \case
  Success a -> Right a
  Failure ne -> Left ne
