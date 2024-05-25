{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.APISpec (spec) where

import OptEnvConf
import Test.Syd

spec :: Spec
spec = do
  exampleParserSpec @Example "example"
  pure ()

exampleParserSpec :: forall a. HasParser a => FilePath -> Spec
exampleParserSpec dir = exampleParserSpec' dir (optEnvParser :: Parser a)

exampleParserSpec' :: FilePath -> Parser a -> Spec
exampleParserSpec' dir parser = do
  it "it documents the parser in the same way" $
    pureGoldenStringFile ("test_resources/" <> dir <> "/help.txt") $
      documentParser parser
  it "shows the parser in the same way" $
    pureGoldenStringFile ("test_resources/" <> dir <> "/help.log") $
      showParserABit parser

data Example = Example
  { exampleGreeting :: Maybe String
  }
  deriving (Show)

instance HasParser Example where
  optEnvParser =
    Example
      <$> optionalFirst
        [ strOpt "--greeting",
          envVar "GREETING",
          confVar "greeting"
        ]
