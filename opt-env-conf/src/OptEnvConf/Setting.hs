{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module OptEnvConf.Setting
  ( Setting (..),
    EnvVarSetting (..),
    ConfigValSetting (..),

    -- * Builders
    help,
    metavar,
    argument,
    option,
    switch,
    reader,
    str,
    auto,
    long,
    short,
    env,
    unprefixedEnv,
    conf,
    confWith,
    confWith',
    unprefixedConf,
    unprefixedConfWith,
    unprefixedConfWith',
    name,
    value,
    valueWithShown,
    example,
    shownExample,
    hidden,
    completer,
    Builder (..),
    BuildInstruction (..),
    requiredCapability,
    readSecretCapability,

    -- * Internal
    showSettingABit,
    SettingHash (..),
    hashSetting,
    completeBuilder,
    mapMaybeBuilder,
    emptySetting,
    Metavar,
    Help,
    prefixEnvVarSetting,
    suffixEnvVarSetting,
    prefixConfigValSetting,
    suffixConfigValSettingKey,

    -- ** Capabilities
    Capabilities (..),
    Capability (..),
    allCapabilities,
    enableCapability,
    disableCapability,
    missingCapabilities,
  )
where

import Autodocodec
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import OptEnvConf.Args (Dashed (..), renderDashed)
import OptEnvConf.Casing
import OptEnvConf.Completer
import OptEnvConf.Reader
import Text.Show

type Metavar = String

type Help = String

-- | A setting for parsing and documenting a single value.
data Setting a = Setting
  { -- | Which dashed values are required for parsing
    --
    -- No dashed values means this is an argument.
    settingDasheds :: ![Dashed],
    -- | Which readers should be tried to parse a value from a string
    settingReaders :: ![Reader a],
    -- | Whether the readers should be used to parsed arguments
    settingTryArgument :: !Bool,
    -- | What value to parse when the switch exists.
    --
    -- Nothing means this is not a switch.
    settingSwitchValue :: !(Maybe a),
    -- | Whether the dasheds should be tried together with the readers as
    -- options.
    settingTryOption :: !Bool,
    -- | Which env vars can be read.
    settingEnvVars :: !(Maybe (NonEmpty EnvVarSetting)),
    -- | Which and how to parse config values
    settingConfigVals :: !(Maybe (NonEmpty (ConfigValSetting a))),
    -- | Default value, if none of the above find the setting.
    settingDefaultValue :: !(Maybe (a, String)),
    -- | Example values
    settingExamples :: ![String],
    -- | Whether to hide docs
    settingHidden :: !Bool,
    -- | Which metavar should be show in documentation
    settingMetavar :: !(Maybe Metavar),
    settingHelp :: !(Maybe String),
    settingCompleter :: !(Maybe Completer),
    settingRequiredCapabilities :: !(Set Capability)
  }

-- An 'Ord'-able Setting without giving 'Setting' an 'Eq' instance
newtype SettingHash = SettingHash Int
  deriving (Show, Eq, Ord)

-- We hash only the parts of the setting that have anything to do with how the
-- setting is parsed, not the parts that are for documentation.
hashSetting :: Setting a -> SettingHash
hashSetting Setting {..} =
  SettingHash
    ( 42
        `hashWithSalt` map renderDashed settingDasheds
        `hashWithSalt` settingTryArgument
        `hashWithSalt` length settingReaders
        `hashWithSalt` isJust settingSwitchValue
        `hashWithSalt` settingTryOption
        `hashWithSalt` settingEnvVars
        `hashWithSalt` (NE.map configValSettingPath <$> settingConfigVals)
        `hashWithSalt` (snd <$> settingDefaultValue)
    )

data EnvVarSetting = EnvVarSetting
  { envVarSettingVar :: !String,
    envVarSettingAllowPrefix :: !Bool
  }
  deriving (Show, Eq)

instance Hashable EnvVarSetting where
  hashWithSalt salt EnvVarSetting {..} =
    salt
      `hashWithSalt` envVarSettingVar
      `hashWithSalt` envVarSettingAllowPrefix

prefixEnvVarSetting :: String -> EnvVarSetting -> EnvVarSetting
prefixEnvVarSetting prefix e =
  if envVarSettingAllowPrefix e
    then e {envVarSettingVar = prefix <> envVarSettingVar e}
    else e

suffixEnvVarSetting :: String -> EnvVarSetting -> EnvVarSetting
suffixEnvVarSetting suffix e = e {envVarSettingVar = envVarSettingVar e <> suffix}

data ConfigValSetting a = forall void.
  ConfigValSetting
  { configValSettingPath :: !(NonEmpty String),
    configValSettingAllowPrefix :: !Bool,
    configValSettingCodec :: !(ValueCodec void (Maybe a))
  }

prefixConfigValSetting :: String -> ConfigValSetting a -> ConfigValSetting a
prefixConfigValSetting prefix c =
  if configValSettingAllowPrefix c
    then c {configValSettingPath = prefix NE.<| configValSettingPath c}
    else c

suffixConfigValSettingKey :: String -> ConfigValSetting a -> ConfigValSetting a
suffixConfigValSettingKey suffix c = c {configValSettingPath = suffixPath $ configValSettingPath c}
  where
    suffixPath :: NonEmpty String -> NonEmpty String
    suffixPath (f :| rest) = case NE.nonEmpty rest of
      Nothing -> (f <> suffix) :| []
      Just ne -> f NE.<| suffixPath ne

-- | A 'mempty' 'Setting' to build up a setting from.
emptySetting :: Setting a
emptySetting =
  Setting
    { settingDasheds = [],
      settingReaders = [],
      settingTryArgument = False,
      settingSwitchValue = Nothing,
      settingTryOption = False,
      settingEnvVars = Nothing,
      settingConfigVals = Nothing,
      settingMetavar = Nothing,
      settingHelp = Nothing,
      settingExamples = [],
      settingHidden = False,
      settingDefaultValue = Nothing,
      settingCompleter = Nothing,
      settingRequiredCapabilities = Set.empty
    }

-- | Show a 'Setting' as much as possible, for debugging
showSettingABit :: Setting a -> ShowS
showSettingABit Setting {..} =
  let Setting _ _ _ _ _ _ _ _ _ _ _ _ _ _ = undefined
   in showParen True $
        showString "Setting "
          . showsPrec 11 settingDasheds
          . showString " "
          . showListWith (\_ -> showString "_") settingReaders
          . showString " "
          . showsPrec 11 settingTryArgument
          . showString " "
          . showMaybeWith (\_ -> showString "_") settingSwitchValue
          . showString " "
          . showsPrec 11 settingTryOption
          . showString " "
          . showsPrec 11 settingEnvVars
          . showString " "
          . showMaybeWith (showNonEmptyWith showConfigValSettingABit) settingConfigVals
          . showString " "
          . showMaybeWith (\_ -> showString "_") settingDefaultValue
          . showString " "
          . showsPrec 11 settingExamples
          . showString " "
          . showsPrec 11 settingHidden
          . showString " "
          . showsPrec 11 settingMetavar
          . showString " "
          . showsPrec 11 settingHelp
          . showString " "
          . showMaybeWith (\_ -> showString "_") settingCompleter
          . showString " "
          . showsPrec 11 settingRequiredCapabilities

showConfigValSettingABit :: ConfigValSetting a -> ShowS
showConfigValSettingABit ConfigValSetting {..} =
  showString "ConfigValSetting "
    . showsPrec 11 configValSettingPath
    . showString " "
    . showString (showCodecABit configValSettingCodec)

showMaybeWith :: (a -> ShowS) -> Maybe a -> ShowS
showMaybeWith _ Nothing = showString "Nothing"
showMaybeWith func (Just a) = showParen True $ showString "Just " . func a

showNonEmptyWith :: (a -> ShowS) -> NonEmpty a -> ShowS
showNonEmptyWith func (a :| as) =
  showParen True $
    func a
      . showString " :| "
      . showListWith func as

-- | Builder for a 'Setting'
newtype Builder a = Builder {unBuilder :: [BuildInstruction a]}

data BuildInstruction a
  = BuildAddHelp !String
  | BuildSetMetavar !String
  | BuildTryArgument
  | BuildTryOption
  | BuildSetSwitchValue !a
  | BuildAddReader !(Reader a)
  | BuildAddLong !(NonEmpty Char)
  | BuildAddShort !Char
  | BuildAddEnv !EnvVarSetting
  | BuildAddConf !(ConfigValSetting a)
  | BuildSetDefault !a !String
  | BuildAddExample !String
  | BuildSetHidden
  | BuildSetCompleter !Completer
  | BuildAddRequiredCapability !Capability

applyBuildInstructions :: [BuildInstruction a] -> Setting a -> Setting a
applyBuildInstructions is s = foldr applyBuildInstruction s is

applyBuildInstruction :: BuildInstruction a -> Setting a -> Setting a
applyBuildInstruction bi s = case bi of
  BuildAddHelp h -> s {settingHelp = Just $ maybe h (<> h) (settingHelp s)}
  BuildSetMetavar mv -> s {settingMetavar = Just mv}
  BuildTryArgument -> s {settingTryArgument = True}
  BuildTryOption -> s {settingTryOption = True}
  BuildSetSwitchValue a -> s {settingSwitchValue = Just a}
  BuildAddReader r -> s {settingReaders = r : settingReaders s}
  BuildAddLong l -> s {settingDasheds = DashedLong l : settingDasheds s}
  BuildAddShort c -> s {settingDasheds = DashedShort c : settingDasheds s}
  BuildAddEnv v -> s {settingEnvVars = Just $ maybe (v :| []) (v <|) $ settingEnvVars s}
  BuildAddConf t -> s {settingConfigVals = Just $ maybe (t :| []) (t <|) $ settingConfigVals s}
  BuildSetDefault a shown -> s {settingDefaultValue = Just (a, shown)}
  BuildAddExample e -> s {settingExamples = e : settingExamples s}
  BuildSetHidden -> s {settingHidden = True}
  BuildSetCompleter c -> s {settingCompleter = Just c}
  BuildAddRequiredCapability c -> s {settingRequiredCapabilities = Set.insert c (settingRequiredCapabilities s)}

instance Semigroup (Builder f) where
  (<>) (Builder f1) (Builder f2) = Builder (f1 <> f2)

instance Monoid (Builder f) where
  mempty = Builder []
  mappend = (<>)

-- | Complete a 'Builder' into a 'Setting'
completeBuilder :: Builder a -> Setting a
completeBuilder b = applyBuildInstructions (unBuilder b) emptySetting

mapMaybeBuilder :: (BuildInstruction a -> Maybe (BuildInstruction b)) -> Builder a -> Builder b
mapMaybeBuilder func = Builder . mapMaybe func . unBuilder

-- | Document a setting
--
-- Multiple 'help's concatenate help on new lines.
help :: String -> Builder a
help s = Builder [BuildAddHelp s]

-- | Document an 'option' or 'env' var.
--
-- Multiple 'metavar's override eachother.
metavar :: String -> Builder a
metavar mv = Builder [BuildSetMetavar mv]

-- | Try to parse an argument.
--
-- You'll also need to add a 'reader'.
--
-- Multiple 'argument's are redundant.
argument :: Builder a
argument = Builder [BuildTryArgument]

-- | Try to parse an argument.
--
-- You'll also need to add a 'reader', at least one 'long' or 'short', and a
-- 'metavar'.
--
-- Multiple 'option's are redundant.
option :: Builder a
option = Builder [BuildTryOption]

-- | Try to parse a switch, activate the given value when succesful
--
-- You'll also need to add at least one 'long' or 'short'.
--
-- Multiple 'switch's override eachother.
switch :: a -> Builder a
switch v = Builder [BuildSetSwitchValue v]

-- | Declare how to parse an argument, option, or environment variable.
reader :: Reader a -> Builder a
reader r = Builder [BuildAddReader r]

-- | Try to parse this 'long' 'option' or 'switch'.
--
-- @long "foo"@ corresponds to @--foo@
--
-- Notes:
--     * Parsing options with an empty name in the 'long' is not supported.
--     * Parsing options with an '=' sign in the 'long' is not supported.
--
-- Multiple 'long's will be tried in order.
-- Empty 'long's will be ignored.
long :: String -> Builder a
long l = Builder [BuildAddLong ne | ne <- maybeToList (NE.nonEmpty l)]

-- | Try to parse this 'short' 'option' or 'switch'.
--
-- @short 'f'@ corresponds to @-f@
--
-- Notes:
--     * Parsing options with @short '-'@ is not supported.
--
-- Multiple 'short's will be tried in order.
short :: Char -> Builder a
short c = Builder [BuildAddShort c]

-- | Try to parse an environment variable.
--
-- You'll also need to add a 'reader' and a 'metavar'.
--
-- Multiple 'env's will be tried in order.
env :: String -> Builder a
env v = Builder [BuildAddEnv (EnvVarSetting v True)]

-- | Like 'env' but ignores any 'subEnv', 'subEnv_', or 'subAll'.
unprefixedEnv :: String -> Builder a
unprefixedEnv v = Builder [BuildAddEnv (EnvVarSetting v False)]

-- | Try to parse a configuration value at the given key.
--
-- Multiple 'conf's will be tried in order.
conf :: (HasCodec a) => String -> Builder a
conf k = confWith k codec

-- | Like 'conf' but with a custom 'Codec' for parsing the value.
confWith :: String -> ValueCodec void a -> Builder a
confWith k c = confWith' k (maybeCodec c)

-- | Like 'confWith' but allows interpreting 'Null' as a value other than "Not found".
confWith' :: String -> ValueCodec void (Maybe a) -> Builder a
confWith' k c =
  let t =
        ConfigValSetting
          { configValSettingPath = k :| [],
            configValSettingAllowPrefix = True,
            configValSettingCodec = c
          }
   in Builder [BuildAddConf t]

-- | Like 'conf' but ignores any 'subConf', 'subConf_', or 'subAll'.
unprefixedConf :: (HasCodec a) => String -> Builder a
unprefixedConf k = unprefixedConfWith k codec

-- | Like 'confWith' but ignores any 'subConf', 'subConf_', or 'subAll'.
unprefixedConfWith :: String -> ValueCodec void a -> Builder a
unprefixedConfWith k c = unprefixedConfWith' k (maybeCodec c)

-- | Like 'confWith'' but ignores any 'subConf', 'subConf_', or 'subAll'.
unprefixedConfWith' :: String -> ValueCodec void (Maybe a) -> Builder a
unprefixedConfWith' k c =
  let t =
        ConfigValSetting
          { configValSettingPath = k :| [],
            configValSettingAllowPrefix = False,
            configValSettingCodec = c
          }
   in Builder [BuildAddConf t]

-- | Short-hand function for 'option', 'long', 'env', and 'conf' at the same time.
--
-- Multiple 'name's will be tried in order.
name :: (HasCodec a) => String -> Builder a
name s =
  mconcat
    [ option,
      long (toArgCase s),
      env (toEnvCase s),
      conf (toConfigCase s)
    ]

-- | Set the default value
--
-- Multiple 'value's override eachother.
--
-- API Note: @default@ is not a valid identifier in Haskell.
-- I'd also have preferred @default@ instead.
value :: (Show a) => a -> Builder a
value = valueWithShown show

-- | Set the default value, along with version of it shown by a custom function.
valueWithShown :: (a -> String) -> a -> Builder a
valueWithShown show' a = Builder [BuildSetDefault a (show' a)]

-- | Provide an example value for documentation.
--
-- The example is provided as a literal string.
--
-- If you use 'reader' 'auto', you'll want to use 'shownExample' instead.
example :: String -> Builder a
example s = Builder [BuildAddExample s]

-- | Use 'Show' to show an 'example'.
--
-- This only makes sense if you use 'reader' 'auto'.
shownExample :: (Show a) => a -> Builder a
shownExample = example . show

-- | Don't show this setting in documentation
--
-- Multiple 'hidden's are redundant.
hidden :: Builder a
hidden = Builder [BuildSetHidden]

-- | Set the setting to tab-complete with the given completer
--
-- Multiple 'completer's are redundant.
completer :: Completer -> Builder a
completer c = Builder [BuildSetCompleter c]

-- | Annotate a setting with a required capability.
requiredCapability :: String -> Builder a
requiredCapability c = Builder [BuildAddRequiredCapability (Capability (T.pack c))]

-- Set of disabled capabilities
newtype Capabilities = Capabilities {unCapabilities :: Set Capability}
  deriving (Show, Generic)

instance Validity Capabilities

allCapabilities :: Capabilities
allCapabilities = Capabilities {unCapabilities = Set.empty}

enableCapability :: Capability -> Capabilities -> Capabilities
enableCapability cap (Capabilities caps) =
  Capabilities (Set.delete cap caps)

disableCapability :: Capability -> Capabilities -> Capabilities
disableCapability cap (Capabilities caps) =
  Capabilities (Set.insert cap caps)

missingCapabilities :: Capabilities -> Set Capability -> Maybe (NonEmpty Capability)
missingCapabilities (Capabilities caps) requiredCapabilities =
  NE.nonEmpty (Set.toList (Set.intersection requiredCapabilities caps))

newtype Capability = Capability {unCapability :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, IsString)

instance Validity Capability

-- | The annotation for any setting reading secrets.
--
-- We add these so that we can disable them in settings checks, to avoid
-- failing settings checks when secrets are read at runtime instead of
-- build-time.
readSecretCapability :: String
readSecretCapability = "read-secret"
