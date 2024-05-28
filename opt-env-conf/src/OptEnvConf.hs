{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

module OptEnvConf
  ( module OptEnvConf,
    module OptEnvConf.Doc,
    module OptEnvConf.Opt,
    module OptEnvConf.Run,
    Parser,
    HasParser (..),
    module Control.Applicative,
  )
where

import Control.Applicative
import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (Dashed (..))
import OptEnvConf.Doc
import OptEnvConf.Opt
import OptEnvConf.Parser
import OptEnvConf.Run

envVar :: String -> Parser (Maybe String)
envVar = ParserEnvVar

strArgument :: [ArgumentBuilder String] -> Parser String
strArgument = argument . (reader str :)

strOption :: [OptionBuilder String] -> Parser (Maybe String)
strOption = option . (reader str :)

argument :: [ArgumentBuilder a] -> Parser a
argument = undefined

option :: [OptionBuilder a] -> Parser (Maybe a)
option = undefined

strArgs :: Parser [String]
strArgs = ParserArgs Nothing

confVar :: String -> Parser (Maybe String)
confVar = ParserConfig

optionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
optionalFirst = ParserOptionalFirst

requiredFirst :: [Parser (Maybe a)] -> Parser a
requiredFirst = ParserRequiredFirst
