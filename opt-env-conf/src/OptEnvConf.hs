{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( argument,
    option,
    switch,
    setting,
    subArgs,
    subEnv,
    subConfig,
    subAll,
    subSettings,
    someNonEmpty,
    mapIO,
    withConfig,
    withYamlConfig,
    xdgYamlConfigFile,
    withLocalYamlConfig,
    enableDisableSwitch,
    runSettingsParser,
    runParser,
    module OptEnvConf.Doc,
    module OptEnvConf.Reader,
    module OptEnvConf.Run,
    module OptEnvConf.Setting,
    Parser,
    HasParser (..),
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
