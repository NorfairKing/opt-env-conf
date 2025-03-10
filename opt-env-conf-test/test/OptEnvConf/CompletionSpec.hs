{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OptEnvConf.CompletionSpec (spec) where

import Data.Maybe
import OptEnvConf.Completer
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
      shouldSuggest
        (setting [short 'e', long "example"])
        0
        []
        ["--example"] -- Only the long version
    it "can complete a short switch from a single dash" $
      shouldSuggest
        (setting [short 'e'])
        0
        ["-"]
        ["-e"]

    it "can complete a long switch from a single dash" $
      shouldSuggest
        (setting [long "example"])
        0
        ["-"]
        ["--example"]

    it "can complete a long switch from a double dash" $
      shouldSuggest
        (setting [long "example"])
        0
        ["--"]
        ["--example"]

    it "can complete a short option with a separate arg" $
      shouldSuggest
        (setting [option, short 'e', completer $ listCompleter ["hi"]])
        1
        ["-e"]
        ["hi"]

    -- Don't think we want to support this.
    -- pending "can complete a short option in short-hand mode.

    it "can complete a long option" $
      shouldSuggest
        (setting [option, long "example", completer $ listCompleter ["hi"]])
        1
        ["--example"]
        ["hi"]

    -- Don't think we want to support this
    -- pending "can complete a long option with equals sign"

    describe "commands" $ do
      let p =
            commands
              [ command "foo" "1" $ pure (),
                command "bar" "2" $ pure (),
                command "baz" "3" $ pure ()
              ]

      it "can complete a command argument" $
        shouldSuggest
          p
          0
          []
          [Completion "foo" (Just "1"), Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "can complete a command argument when it's been partially provided" $
        shouldSuggest
          p
          0
          ["b"]
          [Completion "bar" (Just "2"), Completion "baz" (Just "3")]

    describe "completion after a command" $ do
      it "can complete a command with a switch" $
        shouldSuggest
          (commands [command "foo" "1" $ setting [help "ex", short 'e', long "example"]])
          1
          ["foo"]
          [Completion "--example" (Just "ex")] -- Only the long version
      it "can complete a command's short switch" $
        shouldSuggest
          (commands [command "foo" "1" $ setting [short 'e']])
          1
          ["foo", "-"]
          ["-e"]

      it "can complete a command's long switch from a single dash" $
        shouldSuggest
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "-"]
          ["--example"]

      it "can complete a command's long switch from a double dash" $
        shouldSuggest
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "--"]
          ["--example"]

      it "can complete a command's short option" $
        shouldSuggest
          (commands [command "foo" "1" $ setting [option, short 'e', completer $ listCompleter ["hi"]]])
          2
          ["foo", "-e"]
          ["hi"]

      it "can complete a command's long option" $
        shouldSuggest
          (commands [command "foo" "1" $ setting [option, long "example", completer $ listCompleter ["hi"]]])
          2
          ["foo", "--example"]
          ["hi"]

    it "can complete a file argument" $
      shouldSuggestDesc
        (filePathSetting [help "file arg", argument])
        0
        []
        ["file arg"]

    it "can complete a file option" $
      shouldSuggestDesc
        (filePathSetting [help "file arg", option, long "file"])
        1
        ["--file"]
        ["file arg"]

    it "can complete a directory argument" $
      shouldSuggestDesc
        (directoryPathSetting [help "dir arg", argument])
        0
        []
        ["dir arg"]

    it "can complete a directory option" $
      shouldSuggestDesc
        (directoryPathSetting [help "dir arg", option, long "file"])
        1
        ["--file"]
        ["dir arg"]

shouldSuggest :: Parser a -> Int -> [String] -> [Completion String] -> IO ()
shouldSuggest p ix ws expected = do
  let arg = fromMaybe "" $ listToMaybe $ drop ix ws
  let completions = pureCompletionQuery p ix ws
  evaluatedCompletions <- evalCompletions arg completions
  evaluatedCompletions `shouldBe` expected

shouldSuggestDesc :: Parser a -> Int -> [String] -> [String] -> IO ()
shouldSuggestDesc p ix ws descriptions = do
  let completions = pureCompletionQuery p ix ws
  map completionDescription completions `shouldBe` map Just descriptions
