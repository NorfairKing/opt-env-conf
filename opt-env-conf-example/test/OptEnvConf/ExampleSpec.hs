{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.ExampleSpec where

import OptEnvConf.Completion
import OptEnvConf.Example
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "test_resources/documentation.txt" "opt-env-conf-example"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"

  describe "Settings parsing" $ do
    it "can parse a read dispatch" $
      settingsParserArgsTest ["read"] DispatchRead
    it "can parse default settings from environment variables" $
      settingsParserEnvTest
        []
        Settings
          { settingLogLevel = "DEBUG",
            settingBaseDir = Nothing,
            settingCacheDir = Nothing,
            settingPaymentSettings = Nothing
          }
    it "can parse default settings from an empty config variables" $
      settingsParserConfTest
        []
        Settings
          { settingLogLevel = "DEBUG",
            settingBaseDir = Nothing,
            settingCacheDir = Nothing,
            settingPaymentSettings = Nothing
          }
    it "can parse default settings from an empty everything" $
      settingsParserTest
        []
        []
        Nothing
        Settings
          { settingLogLevel = "DEBUG",
            settingBaseDir = Nothing,
            settingCacheDir = Nothing,
            settingPaymentSettings = Nothing
          }

  describe "Completion" $ do
    it "auto-completes the commands and top-level settings" $
      settingsParserCompletionTest @Instructions
        0
        []
        [ Completion "create" $ Just "Create",
          Completion "read" $ Just "Read",
          Completion "update" $ Just "Update",
          Completion "delete" $ Just "Delete",
          Completion "--log-level" $ Just "minimal severity of log messages",
          Completion "--base" $ Just "base directory for items",
          Completion "--payment-public-key" $ Just "Public key",
          Completion "--payment-secret-key" $ Just "Secret key",
          Completion "--payment-currency" $ Just "Currency",
          Completion "--config-file" $ Just "Path to the configuration file"
        ]
    it "auto-completes the create file option dashed" $
      settingsParserCompletionTest @Instructions
        1
        ["create", "-"]
        [ Completion "--file" $ Just "file to create the item in",
          Completion "--log-level" $ Just "minimal severity of log messages",
          Completion "--base" $ Just "base directory for items",
          Completion "--payment-public-key" $ Just "Public key",
          Completion "--payment-secret-key" $ Just "Secret key",
          Completion "--payment-currency" $ Just "Currency",
          Completion "--config-file" $ Just "Path to the configuration file"
        ]
    it "auto-completes the create file option files" $
      settingsParserCompletionDescriptionTest @Instructions
        2
        ["create", "--file"]
        [ "file to create the item in",
          "minimal severity of log messages",
          "base directory for items",
          "Public key",
          "Secret key",
          "Currency",
          "Path to the configuration file"
        ]
