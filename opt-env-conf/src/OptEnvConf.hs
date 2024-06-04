{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( module OptEnvConf,
    module OptEnvConf.Doc,
    module OptEnvConf.Opt,
    module OptEnvConf.Run,
    module OptEnvConf.Reader,
    Parser,
    optional,
    module Control.Applicative,
  )
where

import Autodocodec
import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.String
import OptEnvConf.Doc
import OptEnvConf.Opt
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run

strArgument :: (IsString string) => [ArgumentBuilder string] -> Parser string
strArgument = argument str

strOption :: (IsString string) => [OptionBuilder string] -> Parser string
strOption = option str

argument :: Reader a -> [ArgumentBuilder a] -> Parser a
argument r = ParserArg r . completeBuilder . mconcat

option :: Reader a -> [OptionBuilder a] -> Parser a
option r = ParserOpt r . completeBuilder . mconcat

envVar :: Reader a -> [EnvBuilder a] -> Parser a
envVar r = ParserEnvVar r . completeBuilder . mconcat

confVal :: (HasCodec a) => String -> Parser a
confVal k = confValWith k codec

confValWith :: String -> ValueCodec void a -> Parser a
confValWith = ParserConfig

optionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
optionalFirst = ParserOptionalFirst

requiredFirst :: [Parser (Maybe a)] -> Parser a
requiredFirst = ParserRequiredFirst

someNonEmpty :: Parser a -> Parser (NonEmpty a)
someNonEmpty = ParserSome

-- TODO make make an OptEnvConfBuilder for this?
optEnvConf :: (HasCodec a) => Reader a -> String -> String -> Parser a
optEnvConf r key h =
  requiredFirst
    [ optional $
        option
          r
          [ long key,
            help h
          ],
      optional $
        envVar
          r
          [ var key
          ], -- TODO just using the key doesn't work, needs to be UPPER_SNAKE_CASE
      optional $ confVal key -- TODO just using the key doesn't work, needs to be kebab-case
    ]

mapIO :: (a -> IO b) -> Parser a -> Parser b
mapIO = ParserMapIO
