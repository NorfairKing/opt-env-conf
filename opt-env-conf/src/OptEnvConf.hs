{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf
  ( module OptEnvConf,
    module Control.Applicative,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as JSON
import OptEnvConf.ArgMap (ArgMap (..))
import qualified OptEnvConf.ArgMap as AM
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EM
import System.Environment (getArgs, getEnvironment)
import System.Exit
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
  ParserOpt :: String -> Parser (Maybe String)
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
    argParserShort :: ![Char],
    argParserLong :: ![String]
  }

envVar :: String -> Parser (Maybe String)
envVar = ParserEnvVar

strArg :: Parser String
strArg = ParserArg

strOpt :: String -> Parser (Maybe String)
strOpt = ParserOpt

confVar :: String -> Parser (Maybe String)
confVar = ParserConfig

optionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
optionalFirst = ParserOptionalFirst

requiredFirst :: [Parser (Maybe a)] -> Parser a
requiredFirst = ParserRequiredFirst

documentParser :: Parser a -> String
documentParser = unlines . go
  where
    go :: Parser a -> [String]
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> []
      ParserAp pf pa -> go pf ++ go pa
      ParserAlt p1 p2 -> go p1 ++ ["or"] ++ go p2
      ParserOptionalFirst ps -> "(optional) first of:" : concatMap go ps
      ParserRequiredFirst ps -> "(required) first of:" : concatMap go ps
      ParserArg -> ["Argument"]
      ParserOpt v -> ["Option: " <> show v]
      ParserEnvVar v -> ["Env var: " <> show v]
      ParserConfig key -> ["Config var: " <> show key]

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
      ParserOpt v -> showParen (d > 10) $ showString "Opt " . showString v
      ParserEnvVar v -> showParen (d > 10) $ showString "EnvVar " . showsPrec 11 v
      ParserConfig key -> showParen (d > 10) $ showString "Config " . showsPrec 11 key

runParser :: Parser a -> IO a
runParser p = do
  args <- AM.parse <$> getArgs
  envVars <- EM.parse <$> getEnvironment
  let mConf = Nothing

  -- TODO do something with the leftovers
  case runParserPure p args envVars mConf of
    Left err -> die err
    Right (a, _) -> pure a

runParserPure ::
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  Either String (a, [String])
runParserPure p args envVars mConfig =
  runExcept $ do
    let ppEnv = PPEnv {ppEnvEnv = envVars, ppEnvConf = mConfig}
    (result, unconsumedArgs) <- runStateT (runReaderT (go p) ppEnv) args
    when (AM.hasUnconsumed unconsumedArgs) $ throwError "Unconsumed args"
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
            Left err -> throwError err -- Error if any fails, don't ignore it.
            Right (mA, s') -> case mA of
              Nothing -> go $ ParserOptionalFirst ps -- Don't record the state and continue to try to parse the next
              Just a -> do
                put s' -- Record the state
                pure (Just a)
      ParserRequiredFirst pss -> case pss of
        [] -> throwError "Required one." -- TODO nicer error
        (p' : ps) -> do
          s <- get
          env <- ask
          case runPP (go p') s env of
            Left err -> throwError err -- Error if any fails, don't ignore it.
            Right (mA, s') -> case mA of
              Nothing -> go $ ParserRequiredFirst ps -- Don't record the state and continue to try to parse the next
              Just a -> do
                put s' -- Record the state of the parser that succeeded
                pure a
      ParserArg -> do
        mA <- ppArg
        case mA of
          Nothing -> throwError "No argument to consume"
          Just a -> pure a
      ParserOpt _ -> undefined
      ParserEnvVar v -> do
        es <- asks ppEnvEnv
        pure (EM.lookup v es)
      ParserConfig key -> do
        mConf <- asks ppEnvConf
        case mConf of
          Nothing -> pure Nothing
          Just conf -> case JSON.parseEither (.: Key.fromString key) conf of
            Left err -> throwError err
            Right v -> pure (Just v)

type PP a = ReaderT PPEnv (StateT ArgMap (Except String)) a

data PPEnv = PPEnv
  { ppEnvEnv :: !EnvMap,
    ppEnvConf :: !(Maybe JSON.Object)
  }

runPP ::
  PP a ->
  ArgMap ->
  PPEnv ->
  Either String (a, ArgMap)
runPP p args envVars =
  runExcept $ runStateT (runReaderT p envVars) args

ppArg :: PP (Maybe String)
ppArg = state AM.consumeArg
