{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.HelpSpec (spec) where

import Control.Applicative
import Data.GenValidity.Aeson ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import OptEnvConf
import OptEnvConf.Args as Args
import OptEnvConf.Args.Gen ()
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Error
import OptEnvConf.TestUtils
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  -- Help parsing works at the top level
  helpSpec
    "toplevel-pure"
    []
  -- Help parsing needs to work even if provided settings are invalid
  helpSpec
    "toplevel-pure-with-invalid-port"
    ["--port", "notaport"]
  helpSpec
    "read"
    ["read"]
  helpSpec
    "create"
    ["create"]
  helpSpec
    "sub"
    ["sub"]
  helpSpec
    "sub-foo"
    ["sub", "foo"]
  helpSpec
    "sub-bar"
    ["sub", "bar"]
  -- Help parsing needs to work if you type in the wrong command too.
  helpSpec
    "wrong-command"
    ["quux"]

data Instructions = Instructions !Settings !Dispatch

instance HasParser Instructions where
  settingsParser =
    withLocalYamlConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchRead
  | DispatchCreate !String
  | DispatchWithSub !Sub

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "read" "read command explanation" $ pure DispatchRead,
        command "create" "create command explanation" $
          DispatchCreate
            <$> setting
              [ help "The item to create",
                reader str,
                argument,
                metavar "STR"
              ],
        command "sub" "command with subcommand" $ DispatchWithSub <$> settingsParser
      ]

data Sub
  = SubFoo
  | SubBar

instance HasParser Sub where
  settingsParser =
    commands
      [ command "foo" "foo explanation" $ pure SubFoo,
        command "bar" "bar explanation" $ pure SubBar
      ]

data Settings = Settings
  { settingPort :: Word16,
    settingLogLevel :: String,
    settingPaymentSettings :: Maybe PaymentSettings
  }

instance HasParser Settings where
  settingsParser = do
    settingPort <-
      setting
        [ help "port to serve requests on",
          reader auto,
          metavar "PORT",
          name "port",
          value 8080
        ]
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
      mapIO readSecretTextFile $
        filePathSetting
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

helpSpec :: String -> [String] -> Spec
helpSpec file args =
  it (unwords ["this help page in the same way for args:", show args]) $ do
    let argMap = Args.parseArgs args
    let parser = settingsParser @Instructions
    errOrDocs <- runHelpParser Nothing argMap parser
    case errOrDocs of
      Left err -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors err
      Right mCommandDoc -> do
        let progname = "example"
        pure $
          pureGoldenChunksFile ("test_resources/help/" <> file <> ".txt") $
            case mCommandDoc of
              Nothing -> renderHelpPage progname "example program description" (parserDocs parser)
              Just (commandPath, commandDoc) -> renderCommandHelpPage progname commandPath commandDoc
