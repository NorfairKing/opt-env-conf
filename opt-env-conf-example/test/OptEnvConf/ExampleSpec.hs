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
