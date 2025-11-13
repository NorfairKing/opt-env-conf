{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Check
  ( runSettingsCheck,
    runSettingsCheckOn,
  )
where

import Control.Monad.Reader hiding (Reader, reader, runReader)
import Control.Monad.State
import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import OptEnvConf.Args as Args
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.Error
import OptEnvConf.NonDet
import OptEnvConf.Parser
import OptEnvConf.Run
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
import OptEnvConf.Validation
import System.Exit
import System.IO (stderr, stdout)
import Text.Colour

runSettingsCheck :: Parser a -> Args -> EnvMap -> IO void
runSettingsCheck p args envVars = do
  stderrTc <- getTerminalCapabilitiesFromHandle stderr
  errOrSets <- runSettingsCheckOn (Just stderrTc) p args envVars Nothing
  case errOrSets of
    Just errs -> do
      hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
      exitFailure
    Nothing -> do
      tc <- getTerminalCapabilitiesFromHandle stdout
      hPutChunksLocaleWith tc stdout ["Settings check successful."]
      exitSuccess

runSettingsCheckOn ::
  Maybe TerminalCapabilities ->
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (Maybe (NonEmpty ParseError))
runSettingsCheckOn mDebugMode parser args envVars mConfig = do
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
    go :: Parser a -> Checker a
    go = \case
      ParserPure a -> liftPP $ ppPure a
      ParserEmpty mLoc -> liftPP $ ppEmpty mLoc
      ParserSetting mLoc set -> liftPP $ ppSetting mLoc set

newtype Checker a = Checker {unChecker :: PP a}
  deriving
    ( Functor,
      Applicative,
      Monad
    )

liftPP :: PP a -> Checker a
liftPP = Checker
