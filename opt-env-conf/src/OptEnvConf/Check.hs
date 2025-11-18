{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Check
  ( runSettingsCheck,
    runSettingsCheckOn,
    CheckResult (..),
  )
where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc)
import OptEnvConf.Args as Args
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.Error
import OptEnvConf.Parser
import OptEnvConf.Run
import OptEnvConf.Setting
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
import System.Exit
import System.IO (stderr, stdout)
import Text.Colour

runSettingsCheck :: Capabilities -> Parser a -> Args -> EnvMap -> Maybe JSON.Object -> IO void
runSettingsCheck capabilities p args envVars mConfig = do
  stderrTc <- getTerminalCapabilitiesFromHandle stderr
  errOrSets <- runSettingsCheckOn capabilities stderrTc p args envVars mConfig
  case errOrSets of
    CheckFailed errs -> do
      hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
      exitFailure
    CheckIncapable missingCaps -> do
      tc <- getTerminalCapabilitiesFromHandle stdout
      hPutChunksLocaleWith tc stdout ["Could not complete parsing settings because of missing capabilities, but no errors were found so far."]
      hPutChunksLocaleWith stderrTc stderr $ renderMissingCapabilities missingCaps
      exitSuccess
    CheckSucceeded _ -> do
      tc <- getTerminalCapabilitiesFromHandle stdout
      hPutChunksLocaleWith tc stdout ["Settings parsed successfully."]
      exitSuccess

renderMissingCapabilities :: NonEmpty MissingCapability -> [Chunk]
renderMissingCapabilities = renderErrors . NE.map capabilityErorr
  where
    capabilityErorr (MissingCapability mLoc cap) =
      ParseError mLoc $ ParseErrorMissingCapability cap

data CheckResult a
  = -- | Check succeeded
    CheckSucceeded a
  | -- | Check could not be completed because of missing capability
    CheckIncapable (NonEmpty MissingCapability)
  | -- | Check failed with parse errors
    CheckFailed (NonEmpty ParseError)
  deriving (Show, Generic, Functor)

data MissingCapability
  = MissingCapability
      -- Where the capability was needed
      !(Maybe SrcLoc)
      -- Where the capability was needed
      !Capability
  deriving (Show, Generic)

runSettingsCheckOn ::
  Capabilities ->
  -- DebugMode, always on
  TerminalCapabilities ->
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  IO (CheckResult a)
runSettingsCheckOn capabilities debugMode p args envVars mConfig = do
  errOrSets <- runParserOn capabilities (Just debugMode) p args envVars mConfig
  pure $ case errOrSets of
    Right a -> CheckSucceeded a
    Left errs ->
      -- If all the errors are missing capability errors, return
      -- CheckIncapable, otherwise CheckFailed
      let mMissingCaps =
            -- This MUST be mapM instead of mapMaybe because we need to ensure
            -- ALL errors are missing capability errors
            mapM
              ( \case
                  ParseError mLoc (ParseErrorMissingCapability cap) -> Just (MissingCapability mLoc cap)
                  _ -> Nothing
              )
              errs
       in case mMissingCaps of
            Just ne -> CheckIncapable ne
            Nothing -> CheckFailed errs
