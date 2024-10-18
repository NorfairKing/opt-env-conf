{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module OptEnvConf
  ( -- * Running parsers
    runSettingsParser,
    HasParser (..),
    Parser,
    runParser,

    -- * Building parsers

    -- ** Settings
    setting,
    filePathSetting,
    directoryPathSetting,

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
    defaultCommand,

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

    -- ** Subparsers
    subSettings,
    allOrNothing,

    -- *** Casing helpers
    toArgCase,
    toEnvCase,
    toConfigCase,

    -- ** Helper functions
    someNonEmpty,
    checkEither,
    checkMaybe,
    checkMapEither,
    checkMapIO,
    checkMapMaybe,
    checkMapEitherForgivable,
    checkMapIOForgivable,
    checkMapMaybeForgivable,
    checkMapMaybe,
    mapIO,
    choice,

    -- ** Loading configuration files
    withConfig,
    withYamlConfig,
    withFirstYamlConfig,
    withCombinedYamlConfigs,
    withCombinedYamlConfigs',
    xdgYamlConfigFile,
    withLocalYamlConfig,
    withConfigurableYamlConfig,
    withoutConfig,

    -- ** Common settings

    -- *** Switches
    enableDisableSwitch,
    yesNoSwitch,
    makeDoubleSwitch,

    -- *** Secrets
    readSecretTextFile,
    secretTextFileSetting,

    -- *** Migration
    strOption,
    strArgument,

    -- ** Readers

    -- *** Common readers
    str,
    auto,
    exists,

    -- *** Constructing your own reader
    maybeReader,
    eitherReader,

    -- *** Comma-separated readers
    commaSeparatedList,
    commaSeparated,
    commaSeparatedSet,

    -- * Re-exports, just in case
    module OptEnvConf.Casing,
    module OptEnvConf.Doc,
    module OptEnvConf.Nix,
    module OptEnvConf.Parser,
    module OptEnvConf.Reader,
    module OptEnvConf.Run,
    module OptEnvConf.Setting,
    module Control.Applicative,
  )
where

import Control.Applicative
import OptEnvConf.Casing
import OptEnvConf.Doc
import OptEnvConf.Nix
import OptEnvConf.Parser
import OptEnvConf.Reader
import OptEnvConf.Run
import OptEnvConf.Setting
