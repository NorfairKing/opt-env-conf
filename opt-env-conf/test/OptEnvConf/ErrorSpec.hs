module OptEnvConf.ErrorSpec (spec) where

import Control.Applicative
import Data.Aeson as JSON (Object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.GenValidity.Aeson ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import OptEnvConf
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..), Opt (..))
import qualified OptEnvConf.ArgMap as ArgMap
import OptEnvConf.ArgMap.Gen ()
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Parser
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
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
          errOrResult <- runParserOn p (ArgMap.parse args) EnvMap.empty Nothing
          case errOrResult of
            Right (a, _) -> expectationFailure $ unlines ["Should not have been able to parse, but did and got:", show a]
            Left errs -> pure $ renderErrors errs

goldenChunksFile :: FilePath -> IO [Chunk] -> GoldenTest Text
goldenChunksFile fp cs =
  goldenTextFile fp $ renderChunksText With24BitColours <$> cs
