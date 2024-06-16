module Main where

import OptEnvConf
import Paths_opt_env_conf_test (version)

main :: IO ()
main = do
  s <- runSettingsParser version
  print (s :: Instructions)

data Instructions = Instructions !Dispatch !Settings
  deriving (Show)

instance HasParser Instructions where
  settingsParser =
    Instructions
      <$> settingsParser
      <*> settingsParser

data Dispatch = Dispatch
  deriving (Show)

instance HasParser Dispatch where
  settingsParser = pure Dispatch

data Settings = Settings
  deriving (Show)

instance HasParser Settings where
  settingsParser = pure Settings
