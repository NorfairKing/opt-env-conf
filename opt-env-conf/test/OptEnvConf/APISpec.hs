{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.APISpec (spec) where

import Data.Text (Text)
import OptEnvConf
import OptEnvConf.Parser
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  exampleParserSpec @Example "example"
  exampleParserSpec @Args "args"
  pure ()

exampleParserSpec :: forall a. HasParser a => FilePath -> Spec
exampleParserSpec dir = exampleParserSpec' dir (optEnvParser :: Parser a)

exampleParserSpec' :: FilePath -> Parser a -> Spec
exampleParserSpec' dir parser = describe dir $ do
  it "it documents the parser in the same way" $
    pureGoldenChunksFile ("test_resources/" <> dir <> "/docs.txt") $
      renderDocs $
        parserDocs parser

  it "it documents the opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/" <> dir <> "/opt.txt") $
      renderCompleteOptDocs $
        parserOptDocs parser

  it "it documents the opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/" <> dir <> "/opt-short.txt") $
      renderShortOptDocs $
        parserOptDocs parser

  it "it documents the opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/" <> dir <> "/opt-long.txt") $
      renderLongOptDocs $
        parserOptDocs parser

  it "it documents the env parser in the same way" $
    pureGoldenTextFile ("test_resources/" <> dir <> "/env.txt") $
      renderEnvDocs $
        parserEnvDocs parser

  it "shows the parser in the same way" $
    pureGoldenStringFile ("test_resources/" <> dir <> "/show.txt") $
      showParserABit parser

pureGoldenChunksFile :: FilePath -> [Chunk] -> GoldenTest Text
pureGoldenChunksFile fp cs =
  pureGoldenTextFile fp $ renderChunksText With24BitColours cs

data Example = Example
  { exampleGreeting :: Maybe String
  }
  deriving (Show)

instance HasParser Example where
  optEnvParser =
    Example
      <$> optionalFirst
        [ strOption [long "greeting"],
          envVar "GREETING",
          confVar "greeting"
        ]

data Args = Args [String]
  deriving (Show)

instance HasParser Args where
  optEnvParser =
    Args
      <$> strArgs
