{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Check
  ( Capabilities (..),
    CheckResult (..),
    runSettingsCheck,
    runSettingsCheckOn,
  )
where

import Control.Monad.Reader hiding (Reader, reader, runReader)
import Control.Monad.State
import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import OptEnvConf.Args as Args
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.Error
import OptEnvConf.NonDet
import OptEnvConf.Output
import OptEnvConf.Parser
import OptEnvConf.Run
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
import OptEnvConf.Validation
import System.Exit
import System.IO (stderr, stdout)
import Text.Colour

runSettingsCheck :: Capabilities -> Parser a -> Args -> EnvMap -> IO void
runSettingsCheck capabilities p args envVars = do
  stderrTc <- getTerminalCapabilitiesFromHandle stderr
  result <- runSettingsCheckOn capabilities (Just stderrTc) p args envVars Nothing
  case result of
    CheckFailed errs -> do
      hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
      exitFailure
    CheckSkipped caps -> do
      tc <- getTerminalCapabilitiesFromHandle stdout
      hPutChunksLocaleWith tc stdout ["Settings check skipped due to missing capabilities, no errors found in the meantime."]
      exitSuccess
    CheckSucceeded _ -> do
      tc <- getTerminalCapabilitiesFromHandle stdout
      hPutChunksLocaleWith tc stdout ["Settings check successful."]
      exitSuccess

runSettingsCheckOn ::
  Capabilities ->
  Maybe TerminalCapabilities ->
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (CheckResult a)
runSettingsCheckOn Capabilities {..} mDebugMode parser args envVars mConfig = do
  let ppState = mkPPState args
  let ppEnv = mkPPEnv envVars mConfig mDebugMode
  let go' = do
        result <- go parser
        leftoverArgs <- liftPP $ gets ppStateArgs
        case recogniseLeftovers leftoverArgs of
          Nothing -> pure result
          Just leftovers -> liftPP $ ppError Nothing $ ParseErrorUnrecognised leftovers
  mTup <- runPPLazy (unChecker go') ppState ppEnv
  case mTup of
    Nothing -> error "TODO figure out when this list can be empty"
    Just ((errOrRes, _), nexts) ->
      let success capsOrResult = case capsOrResult of
            Left missingCaps -> CheckSkipped missingCaps
            Right a -> CheckSucceeded a
       in case errOrRes of
            Success capsOrResult -> pure $ success capsOrResult
            Failure firstErrors ->
              let goNexts ns = do
                    -- TODO: Consider keeping around all errors?
                    mNext <- runNonDetTLazy ns
                    case mNext of
                      Nothing ->
                        pure $
                          CheckFailed $
                            let f = case mDebugMode of
                                  Nothing -> eraseErrorSrcLocs
                                  Just _ -> id
                             in f firstErrors
                      Just ((eOR, _), ns') -> case eOR of
                        Success capsOrResult -> pure $ success capsOrResult
                        Failure _ -> goNexts ns'
               in goNexts nexts
  where
    -- [tag:RunParserCheck]
    -- We need to keep the Run-interpreter for parsers in sync with the
    -- Check-interpreter for parser so that the check parser is useful at all.
    -- Any time you add or change a branch here or here, make sure to add the
    -- appropriate tests in CheckSpec.
    -- This is a bit of a nasty business, so if we can find a better way to do
    -- it, I'll happily accept a PR to do so.
    go :: Parser a -> Checker a
    go = \case
      ParserPure a -> liftPP $ ppPure a
      ParserAp ff fa -> do
        debug [syntaxChunk "Ap"]
        ppIndent $ go ff <*> go fa
      ParserEmpty mLoc -> liftPP $ ppEmpty mLoc
      ParserCheck mLoc forgivable f p' -> do
        debug [syntaxChunk "Parser with check", ": ", mSrcLocChunk mLoc]
        ppIndent $ do
          debug ["parser"]
          a <- ppIndent $ go p'
          debug ["check"]
          ppIndent $ do
            if capabilityAllowIO
              then do
                errOrB <- liftIO $ f a
                case errOrB of
                  Left err -> do
                    debug ["failed, forgivable: ", chunk $ T.pack $ show forgivable]
                    liftPP $ ppError mLoc $ ParseErrorCheckFailed forgivable err
                  Right b -> do
                    debug ["succeeded"]
                    pure b
              else error "TODO"
      ParserSelect fe ff -> do
        debug [syntaxChunk "Select"]
        ppIndent $ select (go fe) (go ff)
      ParserAlt p1 p2 -> do
        debug [syntaxChunk "Alt"]
        ppIndent $ do
          debug ["Trying left side."]
          eor <- ppIndent $ tryChecker (go p1)
          case eor of
            Just a -> do
              debug ["Left side succeeded."]
              pure a
            Nothing -> do
              debug ["Left side failed, trying right side."]
              ppIndent $ go p2
      ParserSetting mLoc set -> liftPP $ ppSetting mLoc set

data CheckResult a
  = CheckSucceeded a
  | CheckFailed (NonEmpty ParseError)
  | CheckSkipped (NonEmpty Capability)

data Capabilities = Capabilities
  { capabilityAllowIO :: !Bool
  }
  deriving (Show, Generic)

instance Validity Capabilities

data Capability = CapabilityAllowIO
  deriving (Show, Generic)

instance Validity Capability

-- Either missing capabilities or a successful result
newtype Checker a = Checker {unChecker :: PP (Either (NonEmpty Capability) a)}

instance Functor Checker where
  fmap f (Checker p) = Checker $ fmap (fmap f) p

instance Applicative Checker where
  pure = Checker . pure . Right

  -- We try to execute both because we want to gather as many errors as
  -- possible, within the PP, before we say "fine" because of capabilities.
  --
  -- We also want to treat PP as an Applicative here to make sure that all the
  -- errors are gathered together instead of one by one.
  Checker pf <*> Checker pa =
    Checker $
      let comb ef ea =
            case (ef, ea) of
              (Right f, Right a) -> Right (f a)
              (Left capsF, Left capsA) -> Left (capsF <> capsA)
              (Left capsF, Right _) -> Left capsF
              (Right _, Left capsA) -> Left capsA
       in comb <$> pf <*> pa

instance Selective Checker where
  -- We try to execute both because we want to gather as many errors as
  -- possible, within the PP, before we say "fine" because of capabilities.
  --
  -- We also want to treat PP as a Selective here to make sure that all the
  -- errors are gathered together instead of one by one.
  select (Checker pe) (Checker pf) =
    Checker $
      let comb ee ef = case (ee, ef) of
            (Right (Left a), Right f) -> Right (f a)
            (Right (Right b), Right _) -> Right b
            (Left capsE, Left capsF) -> Left (capsE <> capsF)
            (Left capsE, Right _) -> Left capsE
            (Right _, Left capsF) -> Left capsF
       in comb <$> pe <*> pf

instance Monad Checker where
  Checker p >>= f = Checker $ do
    eA <- p
    case eA of
      Left caps -> pure (Left caps)
      Right a -> unChecker (f a)

instance MonadIO Checker where
  liftIO = Checker . fmap Right . liftIO

instance MonadReader PPEnv Checker where
  ask = Checker $ fmap Right ask
  local f (Checker p) = Checker (local f p)

liftPP :: PP a -> Checker a
liftPP = Checker . fmap Right

tryChecker :: Checker a -> Checker (Maybe a)
tryChecker (Checker p) = Checker $ do
  res <- tryPP p
  case res of
    Nothing -> pure (Right Nothing)
    Just (Left caps) -> pure (Left caps)
    Just (Right a) -> pure (Right (Just a))
