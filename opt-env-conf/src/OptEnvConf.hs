{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( argument,
    option,
    switch,
    setting,
    subArgs,
    subArgs_,
    subEnv,
    subEnv_,
    subConfig,
    subConfig_,
    subAll,
    subSettings,
    someNonEmpty,
    choice,
    checkMap,
    mapIO,
    commands,
    command,
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
    module OptEnvConf.Casing,
    module OptEnvConf.Setting,
    Parser,
    HasParser (..),
    optional,
    module Control.Applicative,
  )
where

import Control.Applicative
import OptEnvConf.Casing
import OptEnvConf.Doc
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run
import OptEnvConf.Setting
