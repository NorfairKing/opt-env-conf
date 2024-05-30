module OptEnvConf.ErrorSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.Text (Text)
import OptEnvConf
import qualified OptEnvConf.ArgMap as ArgMap
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  parseErrorSpec
    "unrecognised-argument"
    (pure 'a')
    ["arg1", "arg2"]
  parseErrorSpec
    "unrecognised-option-none"
    (pure 'a')
    ["--foo", "bar"]
  parseErrorSpec
    "unrecognised-option-other"
    (strOption [long "foo"])
    ["--quux", "bar"]
  parseErrorSpec
    "missing-argument"
    (strArgument [help "example argument", metavar "ARGUMENT"])
    []
  parseErrorSpec
    "missing-option"
    (strOption [long "foo", help "example option", metavar "FOO"])
    []

parseErrorSpec :: Show a => FilePath -> Parser a -> [String] -> Spec
parseErrorSpec fp p args =
  it (unwords ["renders the", fp, "error the same as before"]) $
    let path = "test_resources/error/" <> fp <> ".txt"
     in goldenChunksFile path $ do
          errOrResult <- runParserComplete p (ArgMap.parse_ args) EnvMap.empty Nothing
          case errOrResult of
            Right a -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
            Left errs -> pure $ renderErrors errs

goldenChunksFile :: FilePath -> IO [Chunk] -> GoldenTest Text
goldenChunksFile fp cs =
  goldenTextFile fp $ renderChunksText With24BitColours <$> cs
