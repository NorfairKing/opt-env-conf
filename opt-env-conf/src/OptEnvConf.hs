{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( strArgument,
    strOption,
    argument,
    option,
    reader,
    switch,
    setting,
    requiredFirst,
    someNonEmpty,
    mapIO,
    withConfig,
    withYamlConfig,
    xdgYamlConfigFile,
    module OptEnvConf.Doc,
    module OptEnvConf.Opt,
    module OptEnvConf.Run,
    module OptEnvConf.Reader,
    Parser,
    optional,
    module Control.Applicative,
  )
where

import Control.Applicative
import OptEnvConf.Doc
import OptEnvConf.Opt
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run
