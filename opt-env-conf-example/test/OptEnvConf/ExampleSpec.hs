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
            settingPaymentSettings = Nothing
          }
    it "can parse default settings from an empty config variables" $
      settingsParserConfTest
        []
        Settings
          { settingLogLevel = "DEBUG",
            settingBaseDir = Nothing,
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
            settingPaymentSettings = Nothing
          }

  describe "Completion" $ do
    let p = settingsParser @Instructions
    it "auto-completes the commands and top-level settings" $
      pureCompletionQuery p 0 []
        `shouldSuggest` [ Completion "create" $ Just "Create",
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
      pureCompletionQuery p 1 ["create", "-"]
        `shouldSuggest` [ Completion "--file" $ Just "file to create the item in",
                          Completion "--log-level" $ Just "minimal severity of log messages",
                          Completion "--base" $ Just "base directory for items",
                          Completion "--payment-public-key" $ Just "Public key",
                          Completion "--payment-secret-key" $ Just "Secret key",
                          Completion "--payment-currency" $ Just "Currency",
                          Completion "--config-file" $ Just "Path to the configuration file"
                        ]
    it "auto-completes the create file option files" $
      case pureCompletionQuery p 2 ["create", "--file"] of
        [] -> expectationFailure "Expected only a file completion, got none"
        [Completion (SuggestionCompleter (Completer _)) (Just "file to create the item in")] -> pure () -- We assume that it's the file completer so we don't have to run this in a temp dir.
        cs -> expectationFailure $ "Expected only a file completion, got more: " <> show (length cs)

shouldSuggest :: [Completion Suggestion] -> [Completion Suggestion] -> IO ()
shouldSuggest cs1 cs2 = do
  s1s <- evalCompletions cs1
  s2s <- evalCompletions cs2
  s1s `shouldBe` s2s
