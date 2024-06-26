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
  parseErrorSpec
    "empty"
    (empty :: Parser String)
    []
  parseErrorSpec
    "missing-argument"
    (setting [reader str, argument, help "example argument", metavar "ARGUMENT"] :: Parser String)
    []
  parseErrorSpec
    "missing-option"
    (setting [reader str, option, long "foo", help "example option", metavar "FOO"] :: Parser String)
    []
  parseErrorSpec
    "read-int-argument"
    (setting [argument, reader auto, help "integer option", metavar "INT"] :: Parser Int)
    ["five"]
  parseErrorSpec
    "read-int-option"
    (setting [option, reader auto, long "num", help "integer option", metavar "INT"] :: Parser Int)
    ["--num", "five"]
  parseErrorSpec
    "some-none"
    (some $ setting [reader str, argument] :: Parser [String])
    []

  parseErrorSpec
    "required-command"
    ( commands
        [ command "one" "first" $ pure '1',
          command "two" "second" $ pure '2'
        ]
    )
    []

  parseErrorSpec
    "unrecognised-command"
    ( commands
        [ command "one" "first" $ pure '1',
          command "two" "second" $ pure '2'
        ]
    )
    ["three"]

  parseErrorSpec
    "unfolding-tombstone-option"
    ( (,)
        <$> setting [option, short 'a', reader str]
        <*> setting [option, short 'b', reader str] ::
        Parser (String, String)
    )
    ["-ba", "foo", "bar"]

  parseErrorSpec
    "unfolding-tombstone-switch"
    ( (,)
        <$> setting [switch (), short 'v']
        <*> setting [option, short 'f', reader str] ::
        Parser ((), String)
    )
    ["-fv", "foo"]

parseErrorSpec :: (HasCallStack) => (Show a) => FilePath -> Parser a -> [String] -> Spec
parseErrorSpec fp p args =
  withFrozenCallStack $
    it (unwords ["renders the", fp, "error the same as before"]) $
      let path = "test_resources/error/" <> fp <> ".txt"
       in goldenChunksFile path $ do
            errOrResult <- runParserOn p (parseArgs args) EnvMap.empty Nothing
            case errOrResult of
              Right a -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
              Left errs -> pure $ renderErrors errs

goldenChunksFile :: FilePath -> IO [Chunk] -> GoldenTest Text
goldenChunksFile fp cs =
  goldenTextFile fp $ renderChunksText With24BitColours <$> cs
