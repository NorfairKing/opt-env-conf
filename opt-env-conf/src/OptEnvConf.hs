{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf
  ( module OptEnvConf,
    module Control.Applicative,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON, (.:))
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
import System.Environment (getArgs, getEnvironment)
import System.Exit
import Text.Colour
import Text.Colour.Layout
import Text.Show

data Parser a where
  -- Functor
  ParserPure :: a -> Parser a
  -- Applicative
  ParserFmap :: (a -> b) -> Parser a -> Parser b
  ParserAp :: Parser (a -> b) -> Parser a -> Parser b
  -- Alternative
  ParserAlt :: Parser a -> Parser a -> Parser a
  -- Combining
  ParserOptionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
  ParserRequiredFirst :: [Parser (Maybe a)] -> Parser a
  -- | Arguments and options
  ParserArg :: Parser String
  ParserArgs :: Parser [String]
  ParserOpt :: String -> Parser (Maybe String)
  ParserArgLeftovers :: Parser [String]
  -- | Env vars
  ParserEnvVar :: String -> Parser (Maybe String)
  -- | Configuration file
  ParserConfig :: FromJSON a => String -> Parser (Maybe a)

instance Functor Parser where
  fmap = ParserFmap

instance Applicative Parser where
  pure = ParserPure
  (<*>) = ParserAp

class HasParser a where
  optEnvParser :: Parser a

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
strOpt = ParserOpt

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

documentParser :: Parser a -> [Chunk]
documentParser = unlinesChunks . go
  where
    go :: Parser a -> [[Chunk]]
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> []
      ParserAp pf pa -> go pf ++ go pa
      ParserAlt p1 p2 -> go p1 ++ [["or"]] ++ go p2
      ParserOptionalFirst ps -> ["(optional) first of:"] : concatMap go ps
      ParserRequiredFirst ps -> ["(required) first of:"] : concatMap go ps
      ParserArg -> [["Argument"]]
      ParserArgs -> [["Arguments"]]
      ParserOpt v -> [["Option: ", chunk $ T.pack $ show v]]
      ParserArgLeftovers -> [["Leftover arguments"]]
      ParserEnvVar v -> [["Env var: ", chunk $ T.pack $ show v]]
      ParserConfig key -> [["Config var: ", chunk $ T.pack $ show key]]

data AnyDocs a
  = AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle ![a]

type OptDocs = AnyDocs OptDoc

data OptDoc
  = OptDocArg !(Maybe String)
  | OptDocArgs !(Maybe String)
  | OptDoc !(NonEmpty Dashed) !(Maybe String)
  | OptDocLeftovers !(Maybe String)

parserOptDocs :: Parser a -> OptDocs
parserOptDocs = go
  where
    go :: Parser a -> OptDocs
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> AnyDocsSingle []
      ParserAp pf pa -> AnyDocsAnd [go pf, go pa]
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserOptionalFirst ps -> AnyDocsOr $ map go ps
      ParserRequiredFirst ps -> AnyDocsOr $ map go ps
      ParserArg -> AnyDocsSingle [OptDocArg Nothing]
      ParserArgs -> AnyDocsSingle [OptDocArgs Nothing]
      ParserOpt _ -> AnyDocsSingle []
      ParserArgLeftovers -> AnyDocsSingle [OptDocLeftovers Nothing]
      ParserEnvVar _ -> AnyDocsSingle []
      ParserConfig _ -> AnyDocsSingle []

renderCompleteOptDocs :: OptDocs -> [Chunk]
renderCompleteOptDocs optDocs =
  unlinesChunks
    [ renderShortOptDocs optDocs,
      renderLongOptDocs optDocs
    ]

renderShortOptDocs :: OptDocs -> [Chunk]
renderShortOptDocs =
  go
    . simplifyAnyDocs
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
                OptDoc flags _ ->
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
      AnyDocsSingle vs ->
        map
          ( \case
              OptDocArg mDoc ->
                [ "ARG",
                  chunk . T.pack $ fromMaybe "undocumented" mDoc
                ]
              OptDocArgs mDoc ->
                [ "[ARG]",
                  chunk . T.pack $ fromMaybe "undocumented" mDoc
                ]
              OptDoc flags mDoc ->
                [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed $ NE.toList flags,
                  chunk . T.pack $ fromMaybe "undocumented" mDoc
                ]
              OptDocLeftovers mDoc ->
                [ "leftovers",
                  chunk . T.pack $ fromMaybe "undocumented" mDoc
                ]
          )
          vs

type EnvDocs = AnyDocs EnvDoc

data EnvDoc = EnvDoc
  { envDocVar :: !String,
    envDocHelp :: !(Maybe String)
  }

parserEnvDocs :: Parser a -> EnvDocs
parserEnvDocs = go
  where
    go :: Parser a -> EnvDocs
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> AnyDocsSingle []
      ParserAp pf pa -> AnyDocsAnd [go pf, go pa]
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserOptionalFirst ps -> AnyDocsOr $ map go ps
      ParserRequiredFirst ps -> AnyDocsOr $ map go ps
      ParserArg -> AnyDocsSingle []
      ParserArgs -> AnyDocsSingle []
      ParserOpt _ -> AnyDocsSingle []
      ParserArgLeftovers -> AnyDocsSingle []
      ParserEnvVar v ->
        AnyDocsSingle
          [ EnvDoc
              { envDocVar = v,
                envDocHelp = Nothing
              }
          ]
      ParserConfig _ -> AnyDocsSingle []

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
      AnyDocsSingle vs ->
        map
          ( \EnvDoc {..} ->
              [ chunk $ T.pack envDocVar,
                chunk . T.pack $ fromMaybe "undocumented" envDocHelp
              ]
          )
          vs

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

showParserABit :: Parser a -> String
showParserABit = ($ "") . go 0
  where
    go :: Int -> Parser a -> ShowS
    go d = \case
      ParserFmap _ p -> showParen (d > 10) $ showString "Fmap _ " . go 11 p
      ParserPure _ -> showParen (d > 10) $ showString "Pure _"
      ParserAp pf pa -> showParen (d > 10) $ showString "Ap " . go 11 pf . go 11 pa
      ParserAlt p1 p2 -> showParen (d > 10) $ showString "Alt " . go 11 p1 . showString " " . go 11 p2
      ParserOptionalFirst ps -> showParen (d > 10) $ showString "OptionalFirst " . showListWith (go 11) ps
      ParserRequiredFirst ps -> showParen (d > 10) $ showString "RequiredFirst " . showListWith (go 11) ps
      ParserArg -> showString "Arg"
      ParserArgs -> showString "Args"
      ParserOpt v -> showParen (d > 10) $ showString "Opt " . showString v
      ParserArgLeftovers -> showString "ArgLeftovers"
      ParserEnvVar v -> showParen (d > 10) $ showString "EnvVar " . showsPrec 11 v
      ParserConfig key -> showParen (d > 10) $ showString "Config " . showsPrec 11 key

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
