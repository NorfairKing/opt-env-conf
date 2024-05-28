{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

module OptEnvConf
  ( module OptEnvConf,
    module OptEnvConf.Run,
    module OptEnvConf.Doc,
    Parser,
    HasParser (..),
    module Control.Applicative,
  )
where

import Control.Applicative
import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (Dashed (..))
import OptEnvConf.Doc
import OptEnvConf.Parser
import OptEnvConf.Run

data ArgParser a = ArgParser
  { argParserParse :: !(String -> Either String a),
    argParserShort :: ![Char], -- TODO use dashed?
    argParserLong :: ![String]
  }

data EnvParser a = EnvParser
  { envParserParse :: !(String -> Either String a),
    envParserVar :: !String
  }

envVar :: String -> Parser (Maybe String)
envVar = ParserEnvVar

strArg :: Parser String
strArg = ParserArg Nothing

strArgs :: Parser [String]
strArgs = ParserArgs Nothing

strOpt :: String -> Parser (Maybe String)
strOpt s =
  ParserOpt
    ( NE.singleton
        . DashedLong
        $ NE.fromList s -- TODO unsafe
    )
    Nothing

argLeftovers :: Parser [String]
argLeftovers = ParserArgLeftovers

-- Arguments _and_ leftovers
allArgs :: Parser [String]
allArgs = (++) <$> strArgs <*> argLeftovers

confVar :: String -> Parser (Maybe String)
confVar = ParserConfig

optionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
optionalFirst = ParserOptionalFirst

requiredFirst :: [Parser (Maybe a)] -> Parser a
requiredFirst = ParserRequiredFirst
