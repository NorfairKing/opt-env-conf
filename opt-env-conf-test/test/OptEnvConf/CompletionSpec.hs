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

    pending "prefers the long setting name over the short one"

    let parser =
          (,)
            <$> setting [long "example"]
            <*> commands
              [ command "foo" "Foo" (setting [long "sub"]),
                command "bar" "Bar" ((,) <$> setting [long "one"] <*> setting [long "two"]),
                command "baz" "Baz" (pure ("baz", "qux"))
              ]
    it "can complete a list of all commands and top-level settings from empty input" $
      pureCompletionQuery parser 1 [] -- [] or [""]? or both?
        `shouldBe` [ Completion "foo" Nothing,
                     Completion "bar" Nothing,
                     Completion "baz" Nothing,
                     Completion "--example" Nothing
                   ]

    it "can complete a list of all commands given a matching prefix" $
      pureCompletionQuery parser 1 ["b"]
        `shouldBe` [ Completion "bar" Nothing,
                     Completion "baz" Nothing
                   ]

    it "can complete the settings specific to a command when the command name is there" $
      pureCompletionQuery parser 1 ["bar "]
        `shouldBe` [ Completion "--one" Nothing,
                     Completion "--two" Nothing
                   ]
