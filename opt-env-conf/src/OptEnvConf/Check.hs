{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Check
  ( runSettingsCheck,
    runSettingsCheckOn,
  )
where

import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import OptEnvConf.Args as Args
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.Error
import OptEnvConf.Parser
import OptEnvConf.Run
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
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
  errsOrResult <- runParserOn mDebugMode parser args envVars mConfig
  case errsOrResult of
    Left errs -> return (Just errs)
    Right _settings -> return Nothing
  where
    go :: Parser a -> Checker a
    go = \case
      ParserPure a -> pure a

newtype Checker a = Checker {unChecker :: PP a}
  deriving
    ( Functor,
      Applicative,
      Monad
    )

liftPP :: PP a -> Checker a
liftPP = Checker
