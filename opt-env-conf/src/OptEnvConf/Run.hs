{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Run where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Validity hiding (Validation)
import GHC.Generics (Generic)
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..))
import qualified OptEnvConf.ArgMap as AM
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EM
import OptEnvConf.Opt
import OptEnvConf.Parser
import System.Environment (getArgs, getEnvironment)
import System.Exit

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
  = ParseErrorEmpty
  | ParseErrorUnconsumed
  | ParseErrorArgumentRead !String
  | ParseErrorOptionRead !String
  | ParseErrorRequired
  | ParseErrorMissingArgument
  | ParseErrorConfigParseError !String
  deriving (Show, Eq)

instance Exception ParseError where
  displayException = \case
    ParseErrorEmpty -> "empty"
    ParseErrorUnconsumed -> "Unconsumed arguments"
    ParseErrorArgumentRead err -> "Unable to read argument: " <> show err
    ParseErrorOptionRead err -> "Unable to read option: " <> show err
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
    tryPP :: PP a -> PP (Either (NonEmpty ParseError) (a, PPState))
    tryPP pp = do
      s <- get
      e <- ask
      pure $ runPP pp s e
    go ::
      Parser a ->
      PP a
    go = \case
      ParserFmap f p' -> f <$> go p'
      ParserPure a -> pure a
      ParserAp ff fa -> go ff <*> go fa
      ParserEmpty -> ppError ParseErrorEmpty
      ParserAlt p1 p2 -> do
        eor <- tryPP (go p1)
        case eor of
          Right (a, s') -> do
            put s'
            pure a
          -- Note that args are not consumed if the alternative failed.
          Left _ -> go p2 -- TODO: Maybe collect the error?
      ParserMany p' -> do
        eor <- tryPP $ go p'
        case eor of
          Left _ -> pure [] -- Err if fails, the end
          Right (a, s') -> do
            put s'
            as <- go (ParserMany p')
            pure (a : as)
      ParserSome p' -> do
        a <- go p'
        as <- go $ ParserMany p'
        pure $ a : as
      ParserOptionalFirst pss -> case pss of
        [] -> pure Nothing
        (p' : ps) -> do
          eor <- tryPP $ go p'
          case eor of
            Left err -> ppErrors err -- Error if any fails, don't ignore it.
            Right (mA, s') -> case mA of
              Nothing -> go $ ParserOptionalFirst ps -- Don't record the state and continue to try to parse the next
              Just a -> do
                put s' -- Record the state
                pure (Just a)
      ParserRequiredFirst pss -> case pss of
        [] -> ppError ParseErrorRequired
        (p' : ps) -> do
          eor <- tryPP $ go p'
          case eor of
            Left err -> ppErrors err -- Error if any fails, don't ignore it.
            Right (mA, s') -> case mA of
              Nothing -> go $ ParserRequiredFirst ps -- Don't record the state and continue to try to parse the next
              Just a -> do
                put s' -- Record the state of the parser that succeeded
                pure a
      ParserArg r _ -> do
        mS <- ppArg
        case mS of
          Nothing -> ppError ParseErrorMissingArgument
          Just s -> case r s of
            Left err -> ppError $ ParseErrorArgumentRead err
            Right a -> pure a
      ParserOpt r o -> do
        let ds = optionSpecificsDasheds $ optionGeneralSpecifics o
        let goD d = do
              mS <- ppOpt d
              forM mS $ \s ->
                case r s of
                  Left err -> ppError $ ParseErrorOptionRead err
                  Right a -> pure a

        let goDs = \case
              [] -> pure Nothing
              (d : rest) -> do
                eor <- tryPP $ goD d
                case eor of
                  Left err -> undefined
                  Right (Nothing, _) -> goDs rest -- Don't record the state and continue with the other options
                  Right (Just a, s') -> do
                    put s'
                    pure $ Just a
        goDs ds
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

type PP a = ReaderT PPEnv (StateT PPState (Validation ParseError)) a

type PPState = ArgMap

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

ppOpt :: Dashed -> PP (Maybe String)
ppOpt d = state $ AM.consumeOpt d

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
