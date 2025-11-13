{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Check
  ( Capabilities (..),
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
  errOrSets <- runSettingsCheckOn capabilities (Just stderrTc) p args envVars Nothing
  case errOrSets of
    Just errs -> do
      hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
      exitFailure
    Nothing -> do
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
  IO (Maybe (NonEmpty ParseError))
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
    Just ((errOrRes, _), nexts) -> case errOrRes of
      Success _ -> pure Nothing
      Failure firstErrors ->
        let goNexts ns = do
              -- TODO: Consider keeping around all errors?
              mNext <- runNonDetTLazy ns
              case mNext of
                Nothing ->
                  pure $
                    Just $
                      let f = case mDebugMode of
                            Nothing -> eraseErrorSrcLocs
                            Just _ -> id
                       in f firstErrors
                Just ((eOR, _), ns') -> case eOR of
                  Success _ -> pure Nothing
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

data Capabilities = Capabilities
  { capabilityAllowIO :: !Bool
  }
  deriving (Show, Generic)

instance Validity Capabilities

newtype Checker a = Checker {unChecker :: PP a}
  deriving
    ( Functor,
      Applicative,
      Selective,
      Monad,
      MonadIO,
      MonadReader PPEnv,
      MonadState PPState
    )

liftPP :: PP a -> Checker a
liftPP = Checker

tryChecker :: Checker a -> Checker (Maybe a)
tryChecker (Checker p) = Checker $ tryPP p
