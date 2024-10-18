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
import Paths_opt_env_conf_example (version)

exampleMain :: IO ()
exampleMain = do
  s <-
    runSettingsParser
      version
      "Example opt-env-conf-based application"
  print (s :: Instructions)

data Instructions = Instructions !Settings !Dispatch
  deriving (Show)

instance HasParser Instructions where
  settingsParser =
    withLocalYamlConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchCreate !String
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
  { settingLogLevel :: String,
    settingPaymentSettings :: Maybe PaymentSettings
  }
  deriving (Show, Eq)

instance HasParser Settings where
  settingsParser = do
    settingLogLevel <-
      setting
        [ help "minimal severity of log messages",
          reader str,
          metavar "LOG_LEVEL",
          name "log-level",
          value "DEBUG"
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
