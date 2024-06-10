{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( argument,
    option,
    switch,
    setting,
    prefixed,
    subConfig,
    requiredFirst,
    someNonEmpty,
    mapIO,
    withConfig,
    withYamlConfig,
    xdgYamlConfigFile,
    module OptEnvConf.Doc,
    module OptEnvConf.Reader,
    module OptEnvConf.Run,
    module OptEnvConf.Setting,
    Parser,
    optional,
    module Control.Applicative,
  )
where

import Control.Applicative
import OptEnvConf.Doc
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run
import OptEnvConf.Setting
