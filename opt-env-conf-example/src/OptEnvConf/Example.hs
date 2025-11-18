{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Example
  ( exampleMain,
    Instructions (..),
    Dispatch (..),
    Settings (..),
  )
where

import Data.Text (Text)
import OptEnvConf
import Path
import Paths_opt_env_conf_example (version)
import System.Exit

exampleMain :: IO ()
exampleMain = do
  s <-
    runSettingsParser
      version
      "Example opt-env-conf-based application"
  print (s :: Instructions)

data Instructions = Instructions !Dispatch !Settings
  deriving (Show)

instance HasParser Instructions where
  settingsParser =
    withLocalYamlConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchCreate !String !(Maybe (Path Abs File))
  | DispatchRead
  | DispatchUpdate !String !String
  | DispatchDelete
  deriving (Show, Eq)

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
              ]
            <*> optional
              ( filePathSetting
                  [ help "file to create the item in",
                    option,
                    short 'f',
                    long "file"
                  ]
              ),
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
        command "delete" "Delete" $
          -- This requireCapability and exitFailure are just here as an example, and used in a check.
          -- [ref:CapabilityCheck]
          -- Normally this would just be "pure DispatchDelete".
          checkWithRequiredCapability "exit" $
            mapIO (const exitFailure) $
              pure DispatchDelete
      ]

data Settings = Settings
  { settingLogLevel :: String,
    settingBaseDir :: !(Maybe (Path Abs Dir)),
    settingCacheDir :: !(Maybe (Path Abs Dir)),
    settingPaymentSettings :: Maybe PaymentSettings
  }
  deriving (Show, Eq)

instance HasParser Settings where
  settingsParser = subEnv "EXAMPLE_" $ subConfig "example" $ do
    settingLogLevel <-
      setting
        [ help "minimal severity of log messages",
          reader str,
          metavar "LOG_LEVEL",
          name "log-level",
          value "DEBUG"
        ]
    settingBaseDir <-
      optional $
        directoryPathSetting
          [ help "base directory for items",
            option,
            short 'b',
            long "base"
          ]
    settingCacheDir <-
      optional $
        directoryPathSetting
          [ help "cache directory",
            unprefixedEnv "CACHE_DIRECTORY",
            unprefixedConf "cache-directory"
          ]
    settingPaymentSettings <- optional $ subSettings "payment"
    pure Settings {..}

data PaymentSettings = PaymentSettings
  { paymentSetPublicKey :: String,
    paymentSetSecretKey :: Text,
    paymentSetCurrency :: Maybe String
  }
  deriving (Show, Eq)

instance HasParser PaymentSettings where
  settingsParser = do
    paymentSetPublicKey <-
      setting
        [ help "Public key",
          reader str,
          name "public-key",
          metavar "PUBLIC_KEY"
        ]
    paymentSetSecretKey <-
      secretTextFileSetting
        [ help "Secret key",
          name "secret-key",
          metavar "SECRET_KEY_FILE"
        ]
    paymentSetCurrency <-
      optional $
        setting
          [ help "Currency",
            reader str,
            name "currency",
            metavar "CURRENCY"
          ]
    pure PaymentSettings {..}
