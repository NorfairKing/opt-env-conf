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
exampleParserSpec' dir parser =
  it "it documents the parser in the same way" $
    pureGoldenStringFile ("test_resources/" <> dir <> "/help.txt") $
      documentParser parser

data Example = Example
  { exampleGreeting :: Maybe String
  }
  deriving (Show)

instance HasParser Example where
  optEnvParser =
    Example
      <$> optional
        ( strOpt "--greeting"
            <|> envVar "GREETING"
            <|> confVar "greeting"
        )
