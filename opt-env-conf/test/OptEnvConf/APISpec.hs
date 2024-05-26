{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.APISpec (spec) where

import OptEnvConf
import Test.Syd

spec :: Spec
spec = do
  exampleParserSpec @Example "example"
  exampleParserSpec @ArgsAndLeftovers "args-and-leftovers"
  pure ()

exampleParserSpec :: forall a. HasParser a => FilePath -> Spec
exampleParserSpec dir = exampleParserSpec' dir (optEnvParser :: Parser a)

exampleParserSpec' :: FilePath -> Parser a -> Spec
exampleParserSpec' dir parser = do
  it "it documents the parser in the same way" $
    pureGoldenStringFile ("test_resources/" <> dir <> "/documentation.txt") $
      documentParser parser
  it "it documents the env parser in the same way" $
    pureGoldenTextFile ("test_resources/" <> dir <> "/env.txt") $
      renderEnvDocs $
        parserEnvDocs parser
  it "shows the parser in the same way" $
    pureGoldenStringFile ("test_resources/" <> dir <> "/show.txt") $
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

data ArgsAndLeftovers = ArgsAndLeftovers [String] [String]
  deriving (Show)

instance HasParser ArgsAndLeftovers where
  optEnvParser =
    ArgsAndLeftovers
      <$> strArgs
      <*> argLeftovers
