{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.LintSpec (spec) where

import Data.Text (Text)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.Lint
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  goldenLintTest
    "null-setting"
    (setting [])
  goldenLintTest
    "empty-setting"
    ( setting
        [ help "Empty setting"
        ]
    )
  goldenLintTest
    "only-default"
    ( setting
        [ help "Only a default value",
          value ()
        ]
    )
  goldenLintTest
    "dash-in-short"
    ( setting
        [ help "Dash in a 'short'",
          switch True,
          short '-'
        ]
    )
  goldenLintTest
    "dash-in-long"
    ( setting
        [ help "Dash in a 'long'",
          switch True,
          long "--switch"
        ]
    )
  goldenLintTest
    "no-reader-for-argument"
    ( setting
        [ argument,
          metavar "STR",
          help "Example"
        ]
    )
  goldenLintTest
    "no-metavar-for-argument"
    ( setting
        [ reader str,
          argument,
          help "Example"
        ] ::
        Parser String
    )
  goldenLintTest
    "no-reader-for-option"
    ( setting
        [ option,
          long "example",
          metavar "STR",
          help "Example"
        ]
    )
  goldenLintTest
    "no-dashed-for-option"
    ( setting
        [ reader str,
          option,
          metavar "STR",
          help "Example"
        ] ::
        Parser String
    )
  goldenLintTest
    "no-option-or-switch-for-dashed"
    ( setting
        [ reader str,
          argument,
          long "hi",
          metavar "STR",
          help "Example"
        ] ::
        Parser String
    )
  goldenLintTest
    "no-metavar-for-option"
    ( setting
        [ reader str,
          long "example",
          option,
          help "Example"
        ] ::
        Parser String
    )
  goldenLintTest
    "no-dashed-for-switch"
    ( setting
        [ switch True,
          help "Example"
        ]
    )
  goldenLintTest
    "no-reader-for-env"
    ( setting
        [ env "EXAMPLE",
          help "Example"
        ]
    )
  goldenLintTest
    "no-metavar-for-env"
    ( setting
        [ reader str,
          env "EXAMPLE",
          help "Example"
        ] ::
        Parser String
    )
  goldenLintTest
    "no-commands"
    (commands [])
  goldenLintTest
    "config-without-load"
    ( setting
        [ help "Config parser with no way to load a config.",
          conf "example"
        ] ::
        Parser String
    )
  goldenLintTest
    "unreadable-example"
    ( setting
        [ help "Example value isn't readable",
          argument,
          reader auto,
          example "NaN",
          metavar "INT"
        ] ::
        Parser Int
    )
  goldenLintTest
    "undecodable-example"
    ( withoutConfig $
        setting
          [ help "Example value isn't decodable",
            conf "example",
            example "NaN"
          ] ::
        Parser Int
    )
  goldenLintTest
    "many-env"
    ( many $
        setting
          [ help "many with setting that always succeeds",
            reader auto,
            env "EXAMPLE",
            metavar "INT"
          ] ::
        Parser [Int]
    )
  goldenLintTest
    "many-conf"
    ( withLocalYamlConfig $
        many $
          setting
            [ help "many with setting that always succeeds",
              conf "example"
            ] ::
        Parser [Int]
    )
  goldenLintTest
    "some-env"
    ( some $
        setting
          [ help "some with setting that always succeeds",
            reader auto,
            env "EXAMPLE",
            metavar "INT"
          ] ::
        Parser [Int]
    )
  goldenLintTest
    "some-conf"
    ( withLocalYamlConfig $
        some $
          setting
            [ help "some with setting that always succeeds",
              conf "example"
            ] ::
        Parser [Int]
    )

goldenLintTest :: (HasCallStack) => FilePath -> Parser a -> Spec
goldenLintTest fp parser = withFrozenCallStack $
  it "produces the same lint error for this parser" $
    goldenChunksFile ("test_resources/lint/" <> fp <> ".txt") $ do
      case lintParser parser of
        Nothing -> expectationFailure "Parser was valid."
        Just errs -> pure $ renderLintErrors errs

goldenChunksFile :: FilePath -> IO [Chunk] -> GoldenTest Text
goldenChunksFile fp cs =
  goldenTextFile fp $ renderChunksText With24BitColours <$> cs
