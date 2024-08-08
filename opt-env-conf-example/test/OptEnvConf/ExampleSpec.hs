{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.ExampleSpec where

import OptEnvConf.Example
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "test_resources/documentation.txt" "opt-env-conf-example"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
  it "can parse a read dispatch" $
    settingsParserArgsTest ["read"] DispatchRead
  it "can parse default settings from environment variables" $
    settingsParserEnvTest
      []
      Settings
        { settingLogLevel = "DEBUG",
          settingPaymentSettings = Nothing
        }
  it "can parse default settings from an empty config variables" $
    settingsParserConfTest
      []
      Settings
        { settingLogLevel = "DEBUG",
          settingPaymentSettings = Nothing
        }
  it "can parse default settings from an empty everything" $
    settingsParserTest
      []
      []
      Nothing
      Settings
        { settingLogLevel = "DEBUG",
          settingPaymentSettings = Nothing
        }
