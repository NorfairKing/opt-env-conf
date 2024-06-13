{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Setting where

import Autodocodec
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (Dashed (..))
import OptEnvConf.Reader
import Text.Show

type Metavar = String

type Help = String

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
    --
    -- Requires at least one Reader.
    settingEnvVars :: !(Maybe (NonEmpty String)),
    -- | Which and how to parse config values
    --
    -- TODO we could actually have value codecs with void as the first argument.
    -- consider doing that.
    settingConfigVals :: !(Maybe (NonEmpty (NonEmpty String, ValueCodec a a))),
    -- | Default value, if none of the above find the setting.
    settingDefaultValue :: Maybe (a, String),
    -- | Whether to hide docs
    settingHidden :: !Bool,
    -- | Which metavar should be show in documentation
    settingMetavar :: !(Maybe Metavar),
    settingHelp :: !(Maybe String)
  }

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
      settingHidden = False,
      settingDefaultValue = Nothing
    }

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
            ( \(k, c) ->
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

newtype Builder a = Builder {unBuilder :: Setting a -> Setting a}

instance Semigroup (Builder f) where
  (<>) (Builder f1) (Builder f2) = Builder (f1 . f2)

instance Monoid (Builder f) where
  mempty = Builder id
  mappend = (<>)

completeBuilder :: Builder a -> Setting a
completeBuilder b = unBuilder b emptySetting

help :: String -> Builder a
help s = Builder $ \op -> op {settingHelp = Just s}

metavar :: String -> Builder a
metavar mv = Builder $ \s -> s {settingMetavar = Just mv}

argument :: Builder a
argument = Builder $ \s -> s {settingTryArgument = True}

option :: Builder a
option = Builder $ \s -> s {settingTryOption = True}

reader :: Reader a -> Builder a
reader r = Builder $ \s -> s {settingReaders = r : settingReaders s}

switch :: a -> Builder a
switch v = Builder $ \s -> s {settingSwitchValue = Just v}

long :: String -> Builder a
long "" = error "Cannot use an empty long-form option."
long l = Builder $ \s -> s {settingDasheds = DashedLong (NE.fromList l) : settingDasheds s}

short :: Char -> Builder a
short c = Builder $ \s -> s {settingDasheds = DashedShort c : settingDasheds s}

env :: String -> Builder a
env v = Builder $ \s -> s {settingEnvVars = Just $ maybe (v :| []) (v <|) $ settingEnvVars s}

conf :: (HasCodec a) => String -> Builder a
conf k = confWith k codec

confWith :: String -> ValueCodec a a -> Builder a
confWith k c =
  let t = (k :| [], c)
   in Builder $ \s -> s {settingConfigVals = Just $ maybe (t :| []) (t <|) $ settingConfigVals s}

-- | Set the default value
value :: (Show a) => a -> Builder a
value a = valueWithShown a (show a)

-- | Set the default value, along with a shown version of it.
valueWithShown :: a -> String -> Builder a
valueWithShown a shown = Builder $ \s -> s {settingDefaultValue = Just (a, shown)}

-- Use the show function in the setting
-- Lint when the show function is absent
-- re-use the show function for 'value'
example :: (Show a) => a -> Builder a
example = help . show

hidden :: Builder a
hidden = Builder $ \s -> s {settingHidden = True}
