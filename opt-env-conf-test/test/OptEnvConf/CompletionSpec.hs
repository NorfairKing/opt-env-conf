{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OptEnvConf.CompletionSpec (spec) where

import Control.Applicative
import qualified Data.List.NonEmpty as NE
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

    describe "commands with a default command" $ do
      let p =
            commands
              [ command "foo" "1" $ pure (),
                command "bar" "2" $ pure (),
                command "baz" "3" $ pure (),
                defaultCommand "bar"
              ]

      it "still lists all commands when there is a default" $
        parserCompletionTest
          p
          0
          []
          [Completion "foo" (Just "1"), Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "still filters commands by prefix when there is a default" $
        parserCompletionTest
          p
          0
          ["b"]
          [Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "completes the default command's switch when no command is given" $
        parserCompletionTest
          ( commands
              [ command "foo" "1" $ setting [switch (), long "example"],
                command "bar" "2" $ pure (),
                defaultCommand "foo"
              ]
          )
          0
          ["--"]
          ["--example"]

      it "completes both commands and the default command's switch" $
        parserCompletionTest
          ( commands
              [ command "foo" "1" $ setting [switch (), long "example"],
                command "bar" "2" $ pure (),
                defaultCommand "foo"
              ]
          )
          0
          []
          [Completion "foo" (Just "1"), Completion "bar" (Just "2"), "--example"]

      it "completes the default command's option when no command is given" $
        parserCompletionTest
          ( commands
              [ command "foo" "1" $ setting [option, long "example", completer $ listCompleter ["hi"]],
                command "bar" "2" $ pure (),
                defaultCommand "foo"
              ]
          )
          0
          ["--"]
          ["--example"]

      it "completes the default command's option value when no command is given" $
        parserCompletionTest
          ( commands
              [ command "foo" "1" $ setting [option, long "example", reader (str :: Reader String), completer $ listCompleter ["hi"]],
                command "bar" "2" $ pure "bar",
                defaultCommand "foo"
              ]
          )
          1
          ["--example"]
          ["hi"]

      it "completes the default command's argument when no command is given" $
        parserCompletionDescriptionTest
          ( commands
              [ command "foo" "1" $ setting [argument, reader (str :: Reader String), help "arg help", completer $ listCompleter ["val"]],
                command "bar" "2" $ pure "bar",
                defaultCommand "foo"
              ]
          )
          0
          []
          -- Should include the default command's argument completer along with command names
          ["1", "2", "arg help"]

      it "completes inside the default command after consuming its switch" $
        parserCompletionTest
          ( commands
              [ command "foo" "1" $
                  (,)
                    <$> setting [switch (), long "first"]
                    <*> setting [switch (), long "second"],
                command "bar" "2" $ pure ((), ()),
                defaultCommand "foo"
              ]
          )
          1
          ["--first"]
          ["--second"]

      it "completes inside the default command after consuming its option" $
        parserCompletionTest
          ( commands
              [ command "foo" "1" $
                  (,)
                    <$> setting [option, reader (str :: Reader String), long "name"]
                    <*> setting [switch (), long "verbose"],
                command "bar" "2" $ pure ("bar", ()),
                defaultCommand "foo"
              ]
          )
          2
          ["--name", "hello"]
          ["--verbose"]

    describe "global options with commands and a default" $ do
      it "completes global options and commands together" $
        parserCompletionTest
          ( (,)
              <$> setting [switch (), long "verbose"]
              <*> commands
                [ command "foo" "1" $ pure (),
                  command "bar" "2" $ pure (),
                  defaultCommand "foo"
                ]
          )
          0
          []
          ["--verbose", Completion "foo" (Just "1"), Completion "bar" (Just "2")]

      it "completes commands after a global option" $
        parserCompletionTest
          ( (,)
              <$> setting [switch (), long "verbose"]
              <*> commands
                [ command "foo" "1" $ pure (),
                  command "bar" "2" $ pure (),
                  defaultCommand "foo"
                ]
          )
          1
          ["--verbose"]
          [Completion "foo" (Just "1"), Completion "bar" (Just "2")]

      it "completes the default command's options after a global option" $
        parserCompletionTest
          ( (,)
              <$> setting [switch (), long "verbose"]
              <*> commands
                [ command "foo" "1" $ setting [switch (), long "example"],
                  command "bar" "2" $ pure (),
                  defaultCommand "foo"
                ]
          )
          1
          ["--verbose"]
          [Completion "foo" (Just "1"), Completion "bar" (Just "2"), "--example"]

    describe "many" $ do
      it "can complete repeated switches" $
        parserCompletionTest
          (length <$> many (setting [switch (), long "verbose", short 'v']))
          0
          []
          ["--verbose"]

      it "can still complete after consuming one" $
        parserCompletionTest
          (length <$> many (setting [switch (), long "verbose", short 'v']))
          1
          ["--verbose"]
          ["--verbose"]

      it "can still complete after consuming two" $
        parserCompletionTest
          (length <$> many (setting [switch (), long "verbose", short 'v']))
          2
          ["-v", "-v"]
          ["--verbose"]

      it "can complete repeated options" $
        parserCompletionTest
          ( many
              ( setting
                  [ option,
                    reader (str :: Reader String),
                    long "include",
                    completer $ listCompleter ["foo", "bar"]
                  ]
              )
          )
          1
          ["--include"]
          ["foo", "bar"]

      it "can complete repeated options after one has already been parsed" $
        parserCompletionTest
          ( many
              ( setting
                  [ option,
                    reader (str :: Reader String),
                    long "include",
                    completer $ listCompleter ["foo", "bar"]
                  ]
              )
          )
          3
          ["--include", "foo", "--include"]
          ["foo", "bar"]

    describe "some" $ do
      it "can complete at least one switch" $
        parserCompletionTest
          (NE.length <$> someNonEmpty (setting [switch (), long "verbose", short 'v']))
          0
          []
          ["--verbose"]

      it "can still complete after consuming one" $
        parserCompletionTest
          (NE.length <$> someNonEmpty (setting [switch (), long "verbose", short 'v']))
          1
          ["--verbose"]
          ["--verbose"]

    describe "allOrNothing" $ do
      it "completes normally inside allOrNothing" $
        parserCompletionTest
          ( allOrNothing $
              (,)
                <$> setting [option, reader (str :: Reader String), long "host"]
                <*> setting [option, reader (str :: Reader String), long "port"]
          )
          0
          []
          ["--host", "--port"]

      it "still suggests the remaining option after one is consumed" $
        parserCompletionTest
          ( allOrNothing $
              (,)
                <$> setting [option, reader (str :: Reader String), long "host"]
                <*> setting [option, reader (str :: Reader String), long "port"]
          )
          2
          ["--host", "localhost"]
          ["--port"]

    describe "check" $ do
      it "completes through a checkMapEither" $
        parserCompletionTest
          ( checkMapEither
              (\s -> if null s then Left "empty" else Right s)
              (setting [argument, reader (str :: Reader String), completer $ listCompleter ["hello"]])
          )
          0
          []
          ["hello"]

      it "completes through a mapIO" $
        parserCompletionTest
          ( mapIO pure $
              setting [switch (), long "example"]
          )
          0
          []
          ["--example"]

    describe "hidden settings" $ do
      it "does not suggest hidden switches" $
        parserCompletionTest
          ( (,)
              <$> setting [switch (), long "visible"]
              <*> setting [switch (), long "secret", hidden]
          )
          0
          []
          ["--visible"]

      it "does not suggest hidden options" $
        parserCompletionTest
          ( (,)
              <$> setting [option, reader (str :: Reader String), long "visible"]
              <*> setting [option, reader (str :: Reader String), long "secret", hidden]
          )
          0
          []
          ["--visible"]

    describe "settings with only env or conf" $ do
      it "does not error on env-only settings combined with arg settings" $
        parserCompletionTest
          ( (,)
              <$> setting [switch (), long "verbose"]
              <*> setting [reader (str :: Reader String), env "MY_VAR"]
          )
          0
          []
          ["--verbose"]

    describe "enableDisableSwitch" $ do
      -- enableDisableSwitch uses a visible "dummy" with the
      -- (enable|disable)- prefix for documentation, while the actual
      -- --enable-feature and --disable-feature switches are hidden.
      it "completes the documented dummy flag" $
        parserCompletionTest
          ( enableDisableSwitch
              [ help "enable feature",
                name "feature",
                value False
              ]
          )
          0
          []
          [Completion "--(enable|disable)-feature" (Just "enable feature")]

      it "filters the documented flag by prefix" $
        parserCompletionTest
          ( enableDisableSwitch
              [ help "enable feature",
                name "feature",
                value False
              ]
          )
          0
          ["--(e"]
          [Completion "--(enable|disable)-feature" (Just "enable feature")]

    describe "choice" $ do
      it "completes across all choices" $
        parserCompletionTest
          ( choice
              [ setting [switch (), long "alpha"],
                setting [switch (), long "beta"],
                setting [switch (), long "gamma"]
              ]
          )
          0
          []
          ["--alpha", "--beta", "--gamma"]

    describe "withDefault" $ do
      it "completes the underlying parser" $
        parserCompletionTest
          (withDefault "default" $ setting [option, reader (str :: Reader String), long "name", completer $ listCompleter ["alice", "bob"]])
          1
          ["--name"]
          ["alice", "bob"]

      it "still suggests the option when it has a default" $
        parserCompletionTest
          (withDefault "default" $ setting [option, reader (str :: Reader String), long "name"])
          0
          []
          ["--name"]

    describe "nested commands" $ do
      let p =
            commands
              [ command "top" "top-level" $
                  commands
                    [ command "sub-a" "sub a" $ pure (),
                      command "sub-b" "sub b" $ pure ()
                    ],
                command "other" "other" $ pure ()
              ]

      it "completes top-level commands" $
        parserCompletionTest
          p
          0
          []
          [Completion "top" (Just "top-level"), Completion "other" (Just "other")]

      it "completes sub-commands after selecting a top-level command" $
        parserCompletionTest
          p
          1
          ["top"]
          [Completion "sub-a" (Just "sub a"), Completion "sub-b" (Just "sub b")]

      it "filters sub-commands by prefix" $
        parserCompletionTest
          p
          1
          ["top", "sub-a"]
          [Completion "sub-a" (Just "sub a")]

      it "completes nested default commands" $
        parserCompletionTest
          ( commands
              [ command "top" "top-level" $
                  commands
                    [ command "sub-a" "sub a" $ setting [switch (), long "flag"],
                      command "sub-b" "sub b" $ pure (),
                      defaultCommand "sub-a"
                    ],
                command "other" "other" $ pure ()
              ]
          )
          1
          ["top"]
          [Completion "sub-a" (Just "sub a"), Completion "sub-b" (Just "sub b"), "--flag"]

    describe "commands merged via alternative" $ do
      it "completes commands from both sides of an alternative" $
        parserCompletionTest
          ( commands [command "foo" "1" $ pure ()]
              <|> commands [command "bar" "2" $ pure ()]
          )
          0
          []
          [Completion "foo" (Just "1"), Completion "bar" (Just "2")]

    describe "arguments after double dash" $ do
      -- A bare -- is consumed as the argument value "--" when it's
      -- the only argument remaining, so completion sees nothing left.
      it "consumes -- as the argument value" $
        parserCompletionTest
          (setting [argument, reader (str :: Reader String), completer $ listCompleter ["file1", "file2"]])
          1
          ["--"]
          []

      -- After --, the completion should suggest positional arguments
      -- rather than switches. Currently it does not: the switch is
      -- still suggested because the completion engine does not
      -- account for -- separating options from arguments.
      it "suggests the switch even after -- (known bug)" $
        parserCompletionTest
          ( (,)
              <$> setting [switch (), long "verbose"]
              <*> setting [argument, reader (str :: Reader String), completer $ listCompleter ["file1", "file2"]]
          )
          2
          ["--", "fi"]
          ["--verbose"]

    describe "mixed arguments and options" $ do
      it "can complete an option after a positional argument" $
        parserCompletionTest
          ( (,)
              <$> setting [argument, reader (str :: Reader String), completer $ listCompleter ["input"]]
              <*> setting [switch (), long "verbose"]
          )
          1
          ["input"]
          ["--verbose"]

      it "can complete a positional after an option" $
        parserCompletionDescriptionTest
          ( (,)
              <$> setting [switch (), long "verbose"]
              <*> setting [argument, reader (str :: Reader String), help "file", completer $ listCompleter ["output"]]
          )
          1
          ["--verbose"]
          ["file"]

    describe "multiple arguments" $ do
      -- When no args have been typed, both argument completers fire
      -- because the completion engine doesn't know which one will be
      -- consumed first. This is the documented TODO in Completion.hs.
      it "completes both arguments when none have been typed" $
        parserCompletionTest
          ( (,)
              <$> setting [argument, reader (str :: Reader String), completer $ listCompleter ["src"]]
              <*> setting [argument, reader (str :: Reader String), completer $ listCompleter ["dst"]]
          )
          0
          []
          ["src", "dst"]

      it "completes the second argument after the first" $
        parserCompletionTest
          ( (,)
              <$> setting [argument, reader (str :: Reader String), completer $ listCompleter ["src"]]
              <*> setting [argument, reader (str :: Reader String), completer $ listCompleter ["dst"]]
          )
          1
          ["something"]
          ["dst"]

    describe "empty parser" $ do
      it "produces no completions for empty" $
        parserCompletionTest
          (empty :: Parser ())
          0
          []
          []

    describe "pure parser" $ do
      it "produces no completions for pure" $
        parserCompletionTest
          (pure () :: Parser ())
          0
          []
          []

    describe "select" $ do
      -- Select uses andCompletions: both branches contribute because
      -- the completion engine cannot know at completion time whether
      -- the first parser will return Left or Right.
      it "completes through a select" $
        parserCompletionTest
          ( select
              (Left <$> setting [argument, reader (str :: Reader String), completer $ listCompleter ["hello"]])
              (const <$> setting [argument, reader (str :: Reader String), completer $ listCompleter ["world"]])
          )
          0
          []
          ["hello", "world"]
