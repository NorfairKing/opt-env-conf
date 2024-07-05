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

parseArgsErrorSpec :: (HasCallStack) => (Show a) => FilePath -> Parser a -> [String] -> Spec
parseArgsErrorSpec fp p args =
  withFrozenCallStack $
    it (unwords ["renders the", fp, "error the same as before"]) $
      let path = "test_resources/error/" <> fp <> ".txt"
       in goldenChunksFile path $ do
            errOrResult <- runParserOn p (parseArgs args) EnvMap.empty Nothing
            case errOrResult of
              Right a -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
              Left errs -> pure $ renderErrors errs

parseEnvErrorSpec :: (HasCallStack) => (Show a) => FilePath -> Parser a -> [(String, String)] -> Spec
parseEnvErrorSpec fp p e =
  withFrozenCallStack $
    it (unwords ["renders the", fp, "error the same as before"]) $
      let path = "test_resources/error/" <> fp <> ".txt"
       in goldenChunksFile path $ do
            errOrResult <- runParserOn p emptyArgs (EnvMap.parse e) Nothing
            case errOrResult of
              Right a -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
              Left errs -> pure $ renderErrors errs

goldenChunksFile :: FilePath -> IO [Chunk] -> GoldenTest Text
goldenChunksFile fp cs =
  goldenTextFile fp $ renderChunksText With24BitColours <$> cs
