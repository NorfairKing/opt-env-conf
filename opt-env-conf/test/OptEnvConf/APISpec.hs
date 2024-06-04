{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.APISpec (spec) where

import Data.Maybe
import Data.Text (Text)
import OptEnvConf
import OptEnvConf.Parser
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  exampleParserSpec "greet" greetParser
  exampleParserSpec "args" argsParser
  exampleParserSpec "optional" optionalParser
  pure ()

exampleParserSpec :: FilePath -> Parser a -> Spec
exampleParserSpec dir parser = describe dir $ do
  it "it documents the parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/docs.txt") $
      renderDocs $
        parserDocs parser

  it "it documents the opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/opt.txt") $
      renderCompleteOptDocs $
        parserOptDocs parser

  it "it documents the opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/opt-short.txt") $
      renderShortOptDocs $
        parserOptDocs parser

  it "it documents the opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/opt-long.txt") $
      renderLongOptDocs $
        parserOptDocs parser

  it "it documents the env parser in the same way" $
    pureGoldenTextFile ("test_resources/docs/" <> dir <> "/env.txt") $
      renderEnvDocs $
        parserEnvDocs parser

  it "shows the parser in the same way" $
    pureGoldenStringFile ("test_resources/docs/" <> dir <> "/show.txt") $
      showParserABit parser

pureGoldenChunksFile :: FilePath -> [Chunk] -> GoldenTest Text
pureGoldenChunksFile fp cs =
  pureGoldenTextFile fp $ renderChunksText With24BitColours cs

data Greet = Greet
  { greetGreetee :: Maybe String,
    greetGreeting :: Maybe String,
    greetPolite :: Bool
  }
  deriving (Show)

greetParser :: Parser Greet
greetParser =
  prefixed "GREET_" $
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
                short 'g',
                metavar "GREETING",
                help "Greeting to use"
              ],
          optional $
            envVar
              str
              [ var "GREETING",
                help "Greeting to use"
              ],
          confVal "greeting"
        ]
      <*> ( fromMaybe False
              <$> optional
                ( switch
                    True
                    [ long "polite",
                      short 'p',
                      help "Whether to be polite"
                    ]
                )
          )

data Args = Args [String]
  deriving (Show)

argsParser :: Parser Args
argsParser =
  Args
    <$> many
      ( strArgument
          [ help "Argument",
            metavar "ARGUMENT"
          ]
      )

data Optional = Optional (Maybe String)

optionalParser :: Parser Optional
optionalParser =
  Optional
    <$> optional
      ( strArgument
          [ help "Argument",
            metavar "ARGUMENT"
          ]
      )
