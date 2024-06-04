{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( strArgument,
    strOption,
    argument,
    option,
    envVar,
    confVal,
    confValWith,
    optionalFirst,
    requiredFirst,
    someNonEmpty,
    optEnvConf,
    mapIO,
    withConfig,
    module OptEnvConf,
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
import OptEnvConf.Doc
import OptEnvConf.Opt
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run

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
