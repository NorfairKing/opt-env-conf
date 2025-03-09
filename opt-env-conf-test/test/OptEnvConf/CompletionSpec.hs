{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OptEnvConf.CompletionSpec (spec) where

import OptEnvConf.Completer
import OptEnvConf.Completion
import OptEnvConf.Parser
import OptEnvConf.Setting
import Path
import Path.IO
import Test.Syd
import Test.Syd.Path

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
        `shouldSuggest` ["-e", "--example"]

    it "can complete a short switch from a single dash" $
      pureCompletionQuery (setting [short 'e']) 0 ["-"]
        `shouldSuggest` ["-e"]

    it "can complete a long switch from a single dash" $
      pureCompletionQuery (setting [long "example"]) 0 ["-"]
        `shouldSuggest` ["--example"]

    it "can complete a long switch from a double dash" $
      pureCompletionQuery (setting [long "example"]) 0 ["--"]
        `shouldSuggest` ["--example"]

    it "can complete a short option with a separate arg" $
      pureCompletionQuery (setting [option, short 'e', completer $ Completer $ pure ["hi"]]) 1 ["-e"]
        `shouldSuggest` ["hi"]

    -- Don't think we want to support this.
    -- pending "can complete a short option in short-hand mode.

    it "can complete a long option" $
      pureCompletionQuery (setting [option, long "example", completer $ Completer $ pure ["hi"]]) 1 ["--example"]
        `shouldSuggest` ["hi"]

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
        pureCompletionQuery p 0 []
          `shouldSuggest` [Completion "foo" (Just "1"), Completion "bar" (Just "2"), Completion "baz" (Just "3")]

      it "can complete a command argument when it's been partially provided" $
        pureCompletionQuery p 0 ["b"]
          `shouldSuggest` [Completion "bar" (Just "2"), Completion "baz" (Just "3")]

    describe "completion after a command" $ do
      it "can complete a command with a switch" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [help "ex", short 'e', long "example"]])
          1
          ["foo"]
          `shouldSuggest` [Completion "-e" (Just "ex"), Completion "--example" (Just "ex")]

      it "can complete a command's short switch" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [short 'e']])
          1
          ["foo", "-"]
          `shouldSuggest` ["-e"]

      it "can complete a command's long switch from a single dash" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "-"]
          `shouldSuggest` ["--example"]

      it "can complete a command's long switch from a double dash" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [long "example"]])
          1
          ["foo", "--"]
          `shouldSuggest` ["--example"]

      it "can complete a command's short option" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [option, short 'e', completer $ Completer $ pure ["hi"]]])
          2
          ["foo", "-e"]
          `shouldSuggest` ["hi"]

      it "can complete a command's long option" $
        pureCompletionQuery
          (commands [command "foo" "1" $ setting [option, long "example", completer $ Completer $ pure ["hi"]]])
          2
          ["foo", "--example"]
          `shouldSuggest` ["hi"]

    -- We have to set the working dir here
    sequential $
      tempDirSpec "opt-env-conf" $ do
        let setupExampleDir tdir = do
              exampleFile1 <- resolveFile tdir "foo.txt"
              writeFile (fromAbsFile exampleFile1) ""
              hiddenFile <- resolveFile tdir ".hidden.txt"
              writeFile (fromAbsFile hiddenFile) ""
              exampleDir <- resolveDir tdir "bar"
              createDir exampleDir
              exampleFile2 <- resolveFile exampleDir "quux.txt"
              writeFile (fromAbsFile exampleFile2) ""
        it "can complete a file argument" $ \tdir ->
          withCurrentDir tdir $ do
            setupExampleDir tdir

            case pureCompletionQuery (filePathSetting [help "file arg", argument]) 0 [] of
              [] -> expectationFailure "Expected only a file completion, got none"
              [Completion (SuggestionCompleter (Completer act)) (Just "file arg")] -> act `shouldReturn` ["foo.txt", "bar"]
              _ -> expectationFailure "Expected only a file completion, got more"

        it "can complete a file option" $ \tdir ->
          withCurrentDir tdir $ do
            setupExampleDir tdir

            case pureCompletionQuery (filePathSetting [help "file arg", option, long "file"]) 1 ["--file"] of
              [] -> expectationFailure "Expected only a file completion, got none"
              [Completion (SuggestionCompleter (Completer act)) (Just "file arg")] -> act `shouldReturn` ["foo.txt", "bar"]
              _ -> expectationFailure "Expected only a file completion, got more"

        it "can complete a directory argument" $ \tdir ->
          withCurrentDir tdir $ do
            setupExampleDir tdir

            case pureCompletionQuery (directoryPathSetting [help "file arg", argument]) 0 [] of
              [] -> expectationFailure "Expected only a file completion, got none"
              [Completion (SuggestionCompleter (Completer act)) (Just "file arg")] -> act `shouldReturn` ["bar"]
              _ -> expectationFailure "Expected only a file completion, got more"

        it "can complete a directory option" $ \tdir ->
          withCurrentDir tdir $ do
            setupExampleDir tdir

            case pureCompletionQuery (directoryPathSetting [help "file arg", option, long "file"]) 1 ["--file"] of
              [] -> expectationFailure "Expected only a file completion, got none"
              [Completion (SuggestionCompleter (Completer act)) (Just "file arg")] -> act `shouldReturn` ["bar"]
              _ -> expectationFailure "Expected only a file completion, got more"

shouldSuggest :: [Completion Suggestion] -> [Completion Suggestion] -> IO ()
shouldSuggest cs1 cs2 = do
  s1s <- evalCompletions cs1
  s2s <- evalCompletions cs2
  s1s `shouldBe` s2s
