{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Setting
  ( Setting (..),

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
    conf,
    confWith,
    confWith',
    name,
    value,
    valueWithShown,
    example,
    shownExample,
    hidden,
    Builder (..),

    -- * Internal
    showSettingABit,
    completeBuilder,
    emptySetting,
    DecodingCodec (..),
    Metavar,
    Help,
  )
where

import Autodocodec
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import OptEnvConf.Args (Dashed (..))
import OptEnvConf.Casing
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
    settingEnvVars :: !(Maybe (NonEmpty String)),
    -- | Which and how to parse config values
    settingConfigVals :: !(Maybe (NonEmpty (NonEmpty String, DecodingCodec a))),
    -- | Default value, if none of the above find the setting.
    settingDefaultValue :: !(Maybe (a, String)),
    -- | Example values
    settingExamples :: ![String],
    -- | Whether to hide docs
    settingHidden :: !Bool,
    -- | Which metavar should be show in documentation
    settingMetavar :: !(Maybe Metavar),
    settingHelp :: !(Maybe String)
  }

data DecodingCodec a = forall void. DecodingCodec (ValueCodec void (Maybe a))

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
      settingDefaultValue = Nothing
    }

-- | Show a 'Setting' as much as possible, for debugging
showSettingABit :: Setting a -> ShowS
showSettingABit Setting {..} =
  showParen True $
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
      . showMaybeWith
        ( showListWith
            ( \(k, DecodingCodec c) ->
                showString "("
                  . shows k
                  . showString ", "
                  . showString (showCodecABit c)
                  . showString ")"
            )
            . NE.toList
        )
        settingConfigVals
      . showString " "
      . showMaybeWith (\_ -> showString "_") settingDefaultValue
      . showString " "
      . showsPrec 11 settingMetavar
      . showString " "
      . showsPrec 11 settingHelp

showMaybeWith :: (a -> ShowS) -> Maybe a -> ShowS
showMaybeWith _ Nothing = showString "Nothing"
showMaybeWith func (Just a) = showParen True $ showString "Just " . func a

-- | Builder for a 'Setting'
newtype Builder a = Builder {unBuilder :: Setting a -> Setting a}

instance Semigroup (Builder f) where
  (<>) (Builder f1) (Builder f2) = Builder (f1 . f2)

instance Monoid (Builder f) where
  mempty = Builder id
  mappend = (<>)

-- | Complete a 'Builder' into a 'Setting'
completeBuilder :: Builder a -> Setting a
completeBuilder b = unBuilder b emptySetting

-- | Document a setting
--
-- Multiple 'help's concatenate help on new lines.
help :: String -> Builder a
help s = Builder $ \op -> op {settingHelp = Just $ maybe s (s <>) (settingHelp op)}

-- | Document an 'option' or 'env' var.
--
-- Multiple 'metavar's override eachother.
metavar :: String -> Builder a
metavar mv = Builder $ \s -> s {settingMetavar = Just mv}

-- | Try to parse an argument.
--
-- You'll also need to add a 'reader'.
--
-- Multiple 'argument's are redundant.
argument :: Builder a
argument = Builder $ \s -> s {settingTryArgument = True}

-- | Try to parse an argument.
--
-- You'll also need to add a 'reader', at least one 'long' or 'short', and a
-- 'metavar'.
--
-- Multiple 'option's are redundant.
option :: Builder a
option = Builder $ \s -> s {settingTryOption = True}

-- | Try to parse a switch, activate the given value when succesful
--
-- You'll also need to add at least one 'long' or 'short'.
--
-- Multiple 'switch's override eachother.
switch :: a -> Builder a
switch v = Builder $ \s -> s {settingSwitchValue = Just v}

-- | Declare how to parse an argument, option, or environment variable.
reader :: Reader a -> Builder a
reader r = Builder $ \s -> s {settingReaders = r : settingReaders s}

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
long l = Builder $ \s -> case NE.nonEmpty l of
  Nothing -> s
  Just ne -> s {settingDasheds = DashedLong ne : settingDasheds s}

-- | Try to parse this 'short' 'option' or 'switch'.
--
-- @short 'f'@ corresponds to @-f@
--
-- Notes:
--     * Parsing options with @short '-'@ is not supported.
--
-- Multiple 'short's will be tried in order.
short :: Char -> Builder a
short c = Builder $ \s -> s {settingDasheds = DashedShort c : settingDasheds s}

-- | Try to parse an environment variable.
--
-- You'll also need to add a 'reader' and a 'metavar'.
--
-- Multiple 'env's will be tried in order.
env :: String -> Builder a
env v = Builder $ \s -> s {settingEnvVars = Just $ maybe (v :| []) (v <|) $ settingEnvVars s}

-- | Try to parse a configuration value at the given key.
--
-- Multiple 'conf's will be tried in order.
conf :: (HasCodec a) => String -> Builder a
conf k = confWith k codec

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

-- | Like 'conf' but with a custom 'Codec' for parsing the value.
confWith :: String -> ValueCodec void a -> Builder a
confWith k c = confWith' k (maybeCodec c)

-- | Like 'confWith' but allows interpreting 'Null' as a value other than "Not found".
confWith' :: String -> ValueCodec void (Maybe a) -> Builder a
confWith' k c =
  let t = (k :| [], DecodingCodec c)
   in Builder $ \s -> s {settingConfigVals = Just $ maybe (t :| []) (t <|) $ settingConfigVals s}

-- | Set the default value
--
-- Multiple 'value's override eachother.
--
-- API Note: @default@ is not a valid identifier in Haskell.
-- I'd also have preferred @default@ instead.
value :: (Show a) => a -> Builder a
value a = valueWithShown a (show a)

-- | Set the default value, along with a shown version of it.
valueWithShown :: a -> String -> Builder a
valueWithShown a shown = Builder $ \s -> s {settingDefaultValue = Just (a, shown)}

-- | Provide an example value for documentation.
--
-- The example is provided as a literal string.
--
-- If you use 'reader' 'auto', you'll want to use 'shownExample' instead.
example :: String -> Builder a
example s = Builder $ \set -> set {settingExamples = s : settingExamples set}

-- | Use 'Show' to show an 'example'.
--
-- This only makes sense if you use 'reader' 'auto'.
shownExample :: (Show a) => a -> Builder a
shownExample = example . show

-- | Don't show this setting in documentation
--
-- Multiple 'hidden's are redundant.
hidden :: Builder a
hidden = Builder $ \s -> s {settingHidden = True}
