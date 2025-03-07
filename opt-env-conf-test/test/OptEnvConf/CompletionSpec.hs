{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OptEnvConf.CompletionSpec (spec) where

import Control.Applicative
import OptEnvConf.Completer
import OptEnvConf.Completion
import OptEnvConf.Parser
import OptEnvConf.Setting
import OptEnvConf.Test
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
      parserCompletionTest
        (setting [switch (), short 'e', long "example"])
        0
        []
        ["--example"] -- Only the long version
    it "can complete a short switch from a single dash" $
      parserCompletionTest
        (setting [switch (), short 'e'])
        0
        ["-"]
        ["-e"]

    it "can complete a long switch from a single dash" $
      parserCompletionTest
        (setting [switch (), long "example"])
        0
        ["-"]
        ["--example"]

    it "can complete a long switch from a double dash" $
      parserCompletionTest
        (setting [switch (), long "example"])
        0
        ["--"]
        ["--example"]

    it "can complete a short option's dashed" $
      parserCompletionTest
        (setting [option, short 'e', completer $ listCompleter ["hi"]])
        0
        []
        ["-e"]

    it "can complete a long option's dashed" $
      parserCompletionTest
        (setting [option, long "example", completer $ listCompleter ["hi"]])
        0
        []
        ["--example"]

    it "can complete a short option with a separate arg" $
      parserCompletionTest
        (setting [option, short 'e', completer $ listCompleter ["hi"]])
        1
        ["-e"]
        ["hi"]

    -- Don't think we want to support this.
    -- pending "can complete a short option in short-hand mode.

    it "can complete a long option" $
      parserCompletionTest
        (setting [option, long "example", completer $ listCompleter ["hi"]])
        1
        ["--example"]
        ["hi"]

    -- Don't think we want to support this
    -- pending "can complete a long option with equals sign"

    it "can complete both switches of a tuple" $
      parserCompletionTest
        ((,) <$> setting [switch (), long "foo"] <*> setting [switch (), long "bar"])
        0
        []
        ["--foo", "--bar"]

    it "can complete both switches of a tuple, with a prefix" $
      parserCompletionTest
        ((,) <$> setting [switch (), long "bar"] <*> setting [switch (), long "baz"])
        0
        ["--b"]
        ["--bar", "--baz"]

    it "can complete both switches of an either" $
      parserCompletionTest
        (setting [switch (), long "foo"] <|> setting [switch (), long "bar"])
        0
        []
        ["--foo", "--bar"]

    it "can complete both switches of an either wrapped in optionals" $
      parserCompletionTest
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
        parserCompletionTest
          p
          0
          []
          [Completion "foo" (Just "1"), Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "can complete a command argument when it's been partially provided" $
        parserCompletionTest
          p
          0
          ["b"]
          [Completion "bar" (Just "2"), Completion "baz" (Just "3")]

    describe "completion after a command" $ do
      it "can complete a command with a switch" $
        parserCompletionTest
          (commands [command "foo" "1" $ setting [help "ex", switch (), short 'e', long "example"]])
          1
          ["foo"]
          [Completion "--example" (Just "ex")] -- Only the long version
      it "can complete a command's short switch" $
        parserCompletionTest
          (commands [command "foo" "1" $ setting [switch (), short 'e']])
          1
          ["foo", "-"]
          ["-e"]

      it "can complete a command's long switch from a single dash" $
        parserCompletionTest
          (commands [command "foo" "1" $ setting [switch (), long "example"]])
          1
          ["foo", "-"]
          ["--example"]

      it "can complete a command's long switch from a double dash" $
        parserCompletionTest
          (commands [command "foo" "1" $ setting [switch (), long "example"]])
          1
          ["foo", "--"]
          ["--example"]

      it "can complete a command's short option" $
        parserCompletionTest
          (commands [command "foo" "1" $ setting [option, short 'e', completer $ listCompleter ["hi"]]])
          2
          ["foo", "-e"]
          ["hi"]

      it "can complete a command's long option" $
        parserCompletionTest
          (commands [command "foo" "1" $ setting [option, long "example", completer $ listCompleter ["hi"]]])
          2
          ["foo", "--example"]
          ["hi"]

    it "can complete a file argument" $
      parserCompletionDescriptionTest
        (filePathSetting [help "file arg", argument])
        0
        []
        ["file arg"]

    it "can complete a file option" $
      parserCompletionDescriptionTest
        (filePathSetting [help "file arg", option, long "file"])
        1
        ["--file"]
        ["file arg"]

    it "can complete a directory argument" $
      parserCompletionDescriptionTest
        (directoryPathSetting [help "dir arg", argument])
        0
        []
        ["dir arg"]

    it "can complete a directory option" $
      parserCompletionDescriptionTest
        (directoryPathSetting [help "dir arg", option, long "file"])
        1
        ["--file"]
        ["dir arg"]

    it "no longer suggests a switch that has already been parsed" $
      parserCompletionTest
        ((,) <$> setting [switch (), long "foo"] <*> setting [switch (), long "bar"])
        1
        ["--foo"]
        ["--bar"]

    it "no longer suggests an option that has already been parsed" $
      parserCompletionTest
        ( (,,)
            <$> setting [option, reader (str :: Reader String), long "foo"]
            <*> setting [option, reader (str :: Reader String), long "bar"]
            <*> setting [switch (), long "quux"]
        )
        2
        ["--foo", "foo"]
        ["--bar", "--quux"]

    it "no longer suggests an argument that has already been parsed" $
      parserCompletionDescriptionTest
        ( (,,)
            <$> setting [argument, reader (str :: Reader String), help "hi", completer $ listCompleter ["hi"]]
            <*> setting [argument, reader (str :: Reader String), help "ho", completer $ listCompleter ["ho"]]
            <*> setting [switch (), long "bar", help "hu"]
        )
        1
        ["foo"]
        ["ho", "hu"]
