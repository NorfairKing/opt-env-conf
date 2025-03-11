{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OptEnvConf.CompletionSpec (spec) where

import Control.Applicative
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
      completionTest
        (setting [short 'e', long "example"])
        0
        []
        ["--example"] -- Only the long version
    it "can complete a short switch from a single dash" $
      completionTest
        (setting [short 'e'])
        0
        ["-"]
        ["-e"]

    it "can complete a long switch from a single dash" $
      completionTest
        (setting [long "example"])
        0
        ["-"]
        ["--example"]

    it "can complete a long switch from a double dash" $
      completionTest
        (setting [long "example"])
        0
        ["--"]
        ["--example"]

    it "can complete a short option with a separate arg" $
      completionTest
        (setting [option, short 'e', completer $ listCompleter ["hi"]])
        1
        ["-e"]
        ["hi"]

    -- Don't think we want to support this.
    -- pending "can complete a short option in short-hand mode.

    it "can complete a long option" $
      completionTest
        (setting [option, long "example", completer $ listCompleter ["hi"]])
        1
        ["--example"]
        ["hi"]

    -- Don't think we want to support this
    -- pending "can complete a long option with equals sign"

    it "can complete both switches of a tuple" $
      completionTest
        ((,) <$> setting [switch (), long "foo"] <*> setting [switch (), long "bar"])
        0
        []
        ["--foo", "--bar"]

    it "can complete both switches of a tuple, with a prefix" $
      completionTest
        ((,) <$> setting [switch (), long "bar"] <*> setting [switch (), long "baz"])
        0
        ["--b"]
        ["--bar", "--baz"]

    it "can complete both switches of an either" $
      completionTest
        (setting [switch (), long "foo"] <|> setting [switch (), long "bar"])
        0
        []
        ["--foo", "--bar"]

    it "can complete both switches of an either wrapped in optionals" $
      completionTest
        (optional (setting [switch (), long "foo"]) <|> optional (setting [switch (), long "bar"]))
        0
        []
        ["--foo", "--bar"]

    describe "commands" $ do
      let p =
            commands
              [ command "foo" "1" $ pure (),
                command "bar" "2" $ pure (),
                command "baz" "3" $ pure ()
              ]

      it "can complete a command argument" $
        completionTest
          p
          0
          []
          [Completion "foo" (Just "1"), Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "can complete a command argument when it's been partially provided" $
        completionTest
          p
          0
          ["b"]
          [Completion "bar" (Just "2"), Completion "baz" (Just "3")]

    describe "completion after a command" $ do
      it "can complete a command with a switch" $
        completionTest
          (commands [command "foo" "1" $ setting [help "ex", short 'e', long "example"]])
          1
          ["foo"]
          [Completion "--example" (Just "ex")] -- Only the long version
      it "can complete a command's short switch" $
        completionTest
          (commands [command "foo" "1" $ setting [short 'e']])
          1
          ["foo", "-"]
          ["-e"]

      it "can complete a command's long switch from a single dash" $
        completionTest
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "-"]
          ["--example"]

      it "can complete a command's long switch from a double dash" $
        completionTest
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "--"]
          ["--example"]

      it "can complete a command's short option" $
        completionTest
          (commands [command "foo" "1" $ setting [option, short 'e', completer $ listCompleter ["hi"]]])
          2
          ["foo", "-e"]
          ["hi"]

      it "can complete a command's long option" $
        completionTest
          (commands [command "foo" "1" $ setting [option, long "example", completer $ listCompleter ["hi"]]])
          2
          ["foo", "--example"]
          ["hi"]

    it "can complete a file argument" $
      completionDescriptionTest
        (filePathSetting [help "file arg", argument])
        0
        []
        ["file arg"]

    it "can complete a file option" $
      completionDescriptionTest
        (filePathSetting [help "file arg", option, long "file"])
        1
        ["--file"]
        ["file arg"]

    it "can complete a directory argument" $
      completionDescriptionTest
        (directoryPathSetting [help "dir arg", argument])
        0
        []
        ["dir arg"]

    it "can complete a directory option" $
      completionDescriptionTest
        (directoryPathSetting [help "dir arg", option, long "file"])
        1
        ["--file"]
        ["dir arg"]

completionTest :: Parser a -> Int -> [String] -> [Completion String] -> IO ()
completionTest p ix ws expected = do
  let arg = fromMaybe "" $ listToMaybe $ drop ix ws
  let completions = pureCompletionQuery p ix ws
  evaluatedCompletions <- evalCompletions arg completions
  evaluatedCompletions `shouldBe` expected

completionDescriptionTest :: Parser a -> Int -> [String] -> [String] -> IO ()
completionDescriptionTest p ix ws descriptions = do
  let completions = pureCompletionQuery p ix ws
  map completionDescription completions `shouldBe` map Just descriptions
