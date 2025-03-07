{-# LANGUAGE OverloadedStrings #-}
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
    it "can complete a switch from nothing" $
      pureCompletionQuery (setting [short 'e', long "example"]) 0 []
        `shouldBe` ["-e", "--example"]
    it "can complete a short switch from a single dash" $
      pureCompletionQuery (setting [short 'e']) 0 ["-"]
        `shouldBe` ["-e"]
    it "can complete a long switch from a single dash" $
      pureCompletionQuery (setting [long "example"]) 0 ["-"]
        `shouldBe` ["--example"]
    it "can complete a long switch from a double dash" $
      pureCompletionQuery (setting [long "example"]) 0 ["--"]
        `shouldBe` ["--example"]

    pending "can complete a short option"
    pending "can complete a long option"
    pending "can complete a long option with equals sign"

    describe "commands" $ do
      let p =
            commands
              [ command "foo" "1" $ pure (),
                command "bar" "2" $ pure (),
                command "baz" "3" $ pure ()
              ]

      it "can complete a command argument" $
        pureCompletionQuery p 0 []
          `shouldBe` [Completion "foo" (Just "1"), Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "can complete a command argument when it's been partially provided" $
        pureCompletionQuery p 0 ["b"]
          `shouldBe` [Completion "bar" (Just "2"), Completion "baz" (Just "3")]

    describe "completion after a command" $ do
      it "can complete a command with a switch" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [help "ex", short 'e', long "example"]])
          1
          ["foo"]
          `shouldBe` [Completion "-e" (Just "ex"), Completion "--example" (Just "ex")]

      it "can complete a command's short switch" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [short 'e']])
          1
          ["foo", "-"]
          `shouldBe` ["-e"]

      it "can complete a command's long switch from a single dash" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "-"]
          `shouldBe` ["--example"]

      it "can complete a command's long switch from a double dash" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "--"]
          `shouldBe` ["--example"]

      pending "can complete a command's short option"
      pending "can complete a command's long option"
      pending "can complete a command's long option with equals sign"

    it "can complete a file argument" $
      pureCompletionQuery (filePathSetting [help "file arg", argument]) 0 []
        `shouldBe` [Completion SuggestionFile (Just "file arg")]

    pending "can complete a file option"

    pending "can complete a director argument"
    pending "can complete a director option"
