{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Check
  ( runSettingsCheck,
  )
where

import OptEnvConf.Args as Args
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.Error
import OptEnvConf.Parser
import OptEnvConf.Run
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
import System.Exit
import System.IO (stderr, stdout)
import Text.Colour

runSettingsCheck :: Capabilities -> Parser a -> Args -> EnvMap -> IO void
runSettingsCheck capabilities p args envVars = do
  stderrTc <- getTerminalCapabilitiesFromHandle stderr
  errOrSets <- runParserOn capabilities (Just stderrTc) p args envVars Nothing
  case errOrSets of
    Left errs -> do
      hPutChunksLocaleWith stderrTc stderr $ renderErrors errs
      exitFailure
    Right _ -> do
      tc <- getTerminalCapabilitiesFromHandle stdout
      hPutChunksLocaleWith tc stdout ["Settings parsed successfully."]
      exitSuccess
