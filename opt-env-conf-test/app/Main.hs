{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import OptEnvConf
import Paths_opt_env_conf_test (version)

main :: IO ()
main = do
  s <- runParser version (pure 'a')
  print s

-- print (s :: Instructions)

data Instructions = Instructions !Settings !Dispatch
  deriving (Show)

instance HasParser Instructions where
  settingsParser =
    Instructions
      <$> settingsParser
      <*> settingsParser

data Dispatch
  = DispatchCreate !String
  | DispatchRead
  | DispatchUpdate !String !String
  | DispatchDelete
  deriving (Show)

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "create" "Create" $
          DispatchCreate
            <$> setting
              [ help "The item to create",
                reader str,
                argument,
                metavar "STR"
              ],
        command "read" "Read" $ pure DispatchRead,
        command "update" "Update" $
          DispatchUpdate
            <$> setting
              [ help "The item identifier of the item to update",
                reader str,
                argument,
                metavar "STR"
              ]
            <*> setting
              [ help "The contents of the item to update",
                reader str,
                argument,
                metavar "STR"
              ],
        command "delete" "Delete" $ pure DispatchDelete
      ]

data Settings = Settings
  { settingLogLevel :: String
  }
  deriving (Show)

instance HasParser Settings where
  settingsParser = do
    settingLogLevel <-
      setting
        [ help "minimal severity of log messages",
          reader str,
          metavar "LOG_LEVEL",
          name "log-level"
        ]
    pure Settings {..}
