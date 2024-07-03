{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( -- * Running parsers
    runParser,
    Parser,
    HasParser (..),
    runSettingsParser,

    -- * Building parsers

    -- ** Settings
    setting,

    -- *** Building settings
    help,
    reader,
    argument,
    option,
    switch,
    long,
    short,
    env,
    conf,
    confWith,
    confWith',
    name,
    value,
    hidden,
    metavar,

    -- ** Commands
    commands,
    command,

    -- ** Composing settings with the usual type-classes
    optional,
    (<$>),
    (<*>),
    (<|>),
    many,
    some,
    select,

    -- ** Prefixing parsers
    subArgs,
    subArgs_,
    subEnv,
    subEnv_,
    subConfig,
    subConfig_,
    subAll,
    subSettings,

    -- *** Casing helpers
    toArgCase,
    toEnvCase,
    toConfigCase,

    -- ** Helper functions
    someNonEmpty,
    checkMap,
    checkMapIO,
    checkMapForgivable,
    mapIO,
    choice,

    -- ** Loading configuration files
    withConfig,
    withYamlConfig,
    xdgYamlConfigFile,
    withLocalYamlConfig,
    withConfigurableYamlConfig,

    -- ** Common use
    enableDisableSwitch,
    yesNoSwitch,
    makeDoubleSwitch,
    readTextSecretFile,

    -- ** Readers
    str,
    auto,
    commaSeparatedList,
    commaSeparated,
    commaSeparatedSet,

    -- * Re-exports, just in case
    module OptEnvConf.Doc,
    module OptEnvConf.Reader,
    module OptEnvConf.Run,
    module OptEnvConf.Casing,
    module OptEnvConf.Setting,
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
