module OptEnvConf.ErrorSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.Text (Text)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.Args as Args
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  parseArgsErrorSpec
    "empty"
    (empty :: Parser String)
    []
  parseArgsErrorSpec
    "missing-argument"
    (setting [reader str, argument, help "example argument", metavar "ARGUMENT"] :: Parser String)
    []
  parseArgsErrorSpec
    "missing-option"
    (setting [reader str, option, long "foo", help "example option", metavar "FOO"] :: Parser String)
    []
  parseArgsErrorSpec
    "missing-options"
    ( (,)
        <$> setting [reader str, option, long "foo", help "example option 1", metavar "FOO"]
        <*> setting [reader str, option, long "bar", help "example option 2", metavar "BAR"] ::
        Parser (String, String)
    )
    []
  parseArgsErrorSpec
    "read-int-argument"
    (setting [argument, reader auto, help "integer option", metavar "INT"] :: Parser Int)
    ["five"]
  parseArgsErrorSpec
    "read-int-option"
    (setting [option, reader auto, long "num", help "integer option", metavar "INT"] :: Parser Int)
    ["--num", "five"]
  parseArgsErrorSpec
    "read-int-options"
    ( (,)
        <$> setting [option, reader auto, long "foo", help "integer option 1", metavar "INT"]
        <*> setting [option, reader auto, long "bar", help "integer option 2", metavar "INT"] ::
        Parser (Int, Int)
    )
    ["--foo", "n", "--bar", "m"]
  parseArgsErrorSpec
    "some-none"
    (some $ setting [reader str, argument] :: Parser [String])
    []

  parseArgsErrorSpec
    "required-command"
    ( commands
        [ command "one" "first" $ pure '1',
          command "two" "second" $ pure '2'
        ]
    )
    []

  parseArgsErrorSpec
    "unrecognised-command"
    ( commands
        [ command "one" "first" $ pure '1',
          command "two" "second" $ pure '2'
        ]
    )
    ["three"]

  parseArgsErrorSpec
    "unfolding-tombstone-option"
    ( (,)
        <$> setting [option, short 'a', reader str]
        <*> setting [option, short 'b', reader str] ::
        Parser (String, String)
    )
    ["-ba", "foo", "bar"]

  parseArgsErrorSpec
    "unfolding-tombstone-switch"
    ( (,)
        <$> setting [switch (), short 'v']
        <*> setting [option, short 'f', reader str] ::
        Parser ((), String)
    )
    ["-fv", "foo"]

  parseEnvErrorSpec
    "missing-var"
    (setting [reader str, env "FOO"] :: Parser String)
    []
  parseEnvErrorSpec
    "missing-vars"
    ( (,)
        <$> setting [reader str, env "FOO"]
        <*> setting [reader str, env "BAR"] ::
        Parser (String, String)
    )
    []

  parseEnvErrorSpec
    "missing-env"
    (setting [reader str, env "FOO"] :: Parser String)
    []
  parseEnvErrorSpec
    "unreadable-var"
    (setting [reader auto, env "FOO"] :: Parser Int)
    [("FOO", "n")]
  parseEnvErrorSpec
    "unreadable-vars"
    ( (,)
        <$> setting [reader auto, env "FOO"]
        <*> setting [reader auto, env "BAR"] ::
        Parser (Int, Int)
    )
    [("FOO", "n"), ("BAR", "m")]

  parseArgsErrorSpec
    "empty-choice"
    (choice [] :: Parser String)
    []

  parseArgsErrorSpec
    "check-failed-checkMaybe"
    (checkMaybe (const Nothing) (setting [argument, reader str, env "FOO", value "bar"]) :: Parser String)
    []
  parseArgsErrorSpec
    "check-failed-checkEither"
    (checkEither (const $ Left "example error") (setting [argument, reader str, env "FOO", value "bar"]) :: Parser String)
    []

  parseArgsErrorSpec
    "all-or-nothing"
    ( choice
        [ allOrNothing $
            (,)
              <$> setting [option, long "foo", reader auto, help "This one will exist", metavar "CHAR"]
              <*> setting [option, long "bar", reader auto, help "This one will not exist", metavar "CHAR"],
          pure ('a', 'b')
        ]
    )
    ["--foo", "'a'"]
  parseArgsErrorSpec
    "all-or-nothing-relevant"
    ( (,)
        <$> choice
          [ allOrNothing $
              (,)
                <$> setting [option, long "foo", reader auto, help "This one will exist", metavar "CHAR"]
                <*> setting [option, long "bar", reader auto, help "This one will not exist", metavar "CHAR"],
            pure ('a', 'b')
          ]
        <*> choice [] ::
        Parser ((Char, Char), Char)
    )
    ["--foo", "'a'"]

  parseArgsErrorSpec
    "unrecognised-arg"
    (pure 'a')
    ["arg"]
  parseArgsErrorSpec
    "unrecognised-option"
    (pure 'b')
    ["--foo", "bar"]
  parseArgsErrorSpec
    "unrecognised-switch"
    (pure 'c')
    ["--foo"]

  parseArgsErrorSpec
    "typo-option"
    (optional $ setting [help "often misspelt as baz", reader str, option, long "bar"] :: Parser (Maybe String))
    ["--baz", "arg"]

  parseArgsErrorSpec
    "typo-switch"
    (optional $ setting [help "often misspelt as baz", switch True, long "bar"] :: Parser (Maybe Bool))
    ["--baz"]

  parseArgsErrorSpec
    "leftover-switch"
    (pure 'a')
    ["--switch"]
  parseArgsErrorSpec
    "leftover-option"
    (pure 'b')
    ["--key", "val"]
  parseArgsErrorSpec
    "leftover-argument"
    (pure 'c')
    ["foo", "bar"]

parseArgsErrorSpec :: (HasCallStack) => (Show a) => FilePath -> Parser a -> [String] -> Spec
parseArgsErrorSpec fp p args =
  withFrozenCallStack $
    it (unwords ["renders the", fp, "error the same as before"]) $
      let path = "test_resources/error/" <> fp <> ".txt"
       in goldenChunksFile path $ do
            errOrResult <- runParserOn p (parseArgs args) EnvMap.empty Nothing
            case errOrResult of
              Right a -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
              Left errs -> pure $ renderErrors $ eraseErrorSrcLocs errs

parseEnvErrorSpec :: (HasCallStack) => (Show a) => FilePath -> Parser a -> [(String, String)] -> Spec
parseEnvErrorSpec fp p e =
  withFrozenCallStack $
    it (unwords ["renders the", fp, "error the same as before"]) $
      let path = "test_resources/error/" <> fp <> ".txt"
       in goldenChunksFile path $ do
            errOrResult <- runParserOn p emptyArgs (EnvMap.parse e) Nothing
            case errOrResult of
              Right a -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
              Left errs -> pure $ renderErrors $ eraseErrorSrcLocs errs

goldenChunksFile :: FilePath -> IO [Chunk] -> GoldenTest Text
goldenChunksFile fp cs =
  goldenTextFile fp $ renderChunksText With24BitColours <$> cs
