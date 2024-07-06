{-# LANGUAGE QuasiQuotes #-}

module OptEnvConf.CompletionSpec (spec) where

import OptEnvConf.Completion
import OptEnvConf.Parser
import OptEnvConf.Setting
import Path
import Test.Syd

spec :: Spec
spec = do
  describe "bash" $ do
    it "produces the same bash completion script" $
      pureGoldenStringFile "test_resources/completion/bash-completion-script.bash" $
        bashCompletionScript [absfile|/usr/bin/example-executable|] "example-executable"
  describe "zsh" $ do
    it "produces the same zsh completion script" $
      pureGoldenStringFile "test_resources/completion/zsh-completion-script.zsh" $
        zshCompletionScript [absfile|/usr/bin/example-executable|] "example-executable"
  describe "fish" $ do
    it "produces the same fish completion script" $
      pureGoldenStringFile "test_resources/completion/fish-completion-script.fish" $
        fishCompletionScript [absfile|/usr/bin/example-executable|] "example-executable"

  describe "pureCompletionQuery" $ do
    it "can complete a short switch from a single dash" $
      pureCompletionQuery (setting [short 'e']) 1 ["-"]
        `shouldBe` [Completion "-e" Nothing]
    it "can complete a long switch from a single dash" $
      pureCompletionQuery (setting [long "example"]) 1 ["-"]
        `shouldBe` [Completion "--example" Nothing]
    it "can complete a long switch from a double dash" $
      pureCompletionQuery (setting [long "example"]) 1 ["--"]
        `shouldBe` [Completion "--example" Nothing]

    pending "can complete a long switch with an equals sign"
