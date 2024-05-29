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
  exampleParserSpec @Greet "greet"
  exampleParserSpec @Args "args"
  exampleParserSpec @Optional "optional"
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

data Greet = Greet
  { greetGreetee :: Maybe String,
    greetGreeting :: Maybe String
  }
  deriving (Show)

instance HasParser Greet where
  optEnvParser =
    Greet
      <$> optional
        ( strArgument
            [ help "Who to greet",
              metavar "SUBJECT"
            ]
        )
      <*> optionalFirst
        [ optional $
            strOption
              [ long "greeting",
                metavar "GREETING",
                help "Greeting to use"
              ],
          envVar "GREETING",
          confVar "greeting"
        ]

data Args = Args [String]
  deriving (Show)

instance HasParser Args where
  optEnvParser =
    Args
      <$> many
        ( strArgument
            [ help "Argument",
              metavar "ARGUMENT"
            ]
        )

data Optional = Optional (Maybe String)

instance HasParser Optional where
  optEnvParser =
    Optional
      <$> optional
        ( strArgument
            [ help "Argument",
              metavar "ARGUMENT"
            ]
        )
