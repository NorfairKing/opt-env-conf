{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Opt where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.String
import OptEnvConf.ArgMap (Dashed (..))
import OptEnvConf.Reader
import Text.Show

type Metavar = String

type Help = String

data Setting f = Setting
  -- TODO Completer
  { settingSpecifics :: f,
    settingHelp :: !(Maybe String)
  }

emptySettingWith :: f -> Setting f
emptySettingWith f =
  Setting
    { settingSpecifics = f,
      settingHelp = Nothing
    }

showSettingABitWith :: (f -> ShowS) -> Setting f -> ShowS
showSettingABitWith func Setting {..} =
  showParen True $
    showString "Setting "
      . func settingSpecifics
      . showString " "
      . showParen
        True
        ( showString "Setting "
            . showsPrec 11 settingHelp
        )

newtype Builder f = Builder {unBuilder :: Setting f -> Setting f}

class CanComplete f where
  completeBuilder :: Builder f -> Setting f

instance Semigroup (Builder f) where
  (<>) (Builder f1) (Builder f2) = Builder (f1 . f2)

instance Monoid (Builder f) where
  mempty = Builder id
  mappend = (<>)

class HasReader a f where
  addReader :: Reader a -> f -> f

instance (HasReader a f) => HasReader a (Setting f) where
  addReader r op = op {settingSpecifics = addReader r (settingSpecifics op)}

class IsSwitch a f where
  setSwitchValue :: a -> f -> f

instance (IsSwitch a f) => IsSwitch a (Setting f) where
  setSwitchValue a op = op {settingSpecifics = setSwitchValue a (settingSpecifics op)}

class HasMetavar a where
  setMetavar :: Metavar -> a -> a

instance (HasMetavar f) => HasMetavar (Setting f) where
  setMetavar v op = op {settingSpecifics = setMetavar v (settingSpecifics op)}

class HasLong a where
  addLong :: NonEmpty Char -> a -> a

instance (HasLong f) => HasLong (Setting f) where
  addLong s op = op {settingSpecifics = addLong s (settingSpecifics op)}

class HasShort a where
  addShort :: Char -> a -> a

instance (HasShort f) => HasShort (Setting f) where
  addShort c op = op {settingSpecifics = addShort c (settingSpecifics op)}

class HasEnvVar a where
  addEnvVar :: String -> a -> a

instance (HasEnvVar f) => HasEnvVar (Setting f) where
  addEnvVar v op = op {settingSpecifics = addEnvVar v (settingSpecifics op)}

data SettingSpecifics a = SettingSpecifics
  { -- | Which dashed values are required for parsing
    --
    -- No dashed values means this is an argument.
    settingSpecificsDasheds :: ![Dashed],
    -- | What value to parse when the switch exists.
    --
    -- Nothing means this is not a switch.
    settingSpecificsSwitchValue :: !(Maybe a),
    -- | How to read a string into a value.
    --
    -- An empty list means it doesn't take an argument.
    settingSpecificsReaders :: ![Reader a],
    -- | Which env vars can be read.
    --
    -- Requires at least one Reader.
    settingSpecificsEnvVars :: ![String],
    -- | Which metavar should be show in documentation
    settingSpecificsMetavar :: !(Maybe Metavar)
  }

instance CanComplete (SettingSpecifics a) where
  completeBuilder b = unBuilder b emptySettingParser

instance HasLong (SettingSpecifics a) where
  addLong s os = os {settingSpecificsDasheds = DashedLong s : settingSpecificsDasheds os}

instance HasShort (SettingSpecifics a) where
  addShort c os = os {settingSpecificsDasheds = DashedShort c : settingSpecificsDasheds os}

instance IsSwitch a (SettingSpecifics a) where
  setSwitchValue a os = os {settingSpecificsSwitchValue = Just a}

instance HasReader a (SettingSpecifics a) where
  addReader a os = os {settingSpecificsReaders = a : settingSpecificsReaders os}

instance HasEnvVar (SettingSpecifics a) where
  addEnvVar v os = os {settingSpecificsEnvVars = v : settingSpecificsEnvVars os}

instance HasMetavar (SettingSpecifics a) where
  setMetavar mv os = os {settingSpecificsMetavar = Just mv}

type SettingParser a = Setting (SettingSpecifics a)

emptySettingParser :: SettingParser a
emptySettingParser =
  emptySettingWith
    SettingSpecifics
      { settingSpecificsDasheds = [],
        settingSpecificsSwitchValue = Nothing,
        settingSpecificsReaders = [],
        settingSpecificsEnvVars = [],
        settingSpecificsMetavar = Nothing
      }

showSettingParserABit :: SettingParser a -> ShowS
showSettingParserABit = showSettingABitWith $ \SettingSpecifics {..} ->
  showString "OptionSpecifics "
    . showsPrec 11 settingSpecificsDasheds
    . showString " "
    . showMaybeWith (\_ -> showString "_") settingSpecificsSwitchValue
    . showString " "
    . showListWith (\_ -> showString "_") settingSpecificsReaders
    . showString " "
    . showsPrec 11 settingSpecificsEnvVars
    . showString " "
    . showsPrec 11 settingSpecificsMetavar

showMaybeWith :: (a -> ShowS) -> Maybe a -> ShowS
showMaybeWith _ Nothing = showString "Nothing"
showMaybeWith func (Just a) = showParen True $ showString "Just " . func a

type SettingBuilder a = Builder (SettingSpecifics a)

help :: String -> Builder f
help s = Builder $ \op -> op {settingHelp = Just s}

metavar :: (HasMetavar f) => String -> Builder f
metavar s = Builder $ setMetavar s

strArgument :: (IsString string) => Builder (SettingSpecifics string)
strArgument = argument str

argument :: Reader a -> Builder (SettingSpecifics a)
argument = reader

strOption :: (IsString string) => Builder (SettingSpecifics string)
strOption = option str

option :: Reader a -> Builder (SettingSpecifics a)
option = reader

reader :: Reader a -> Builder (SettingSpecifics a)
reader b = Builder $ addReader b

switch :: a -> Builder (SettingSpecifics a)
switch v = Builder $ setSwitchValue v

long :: (HasLong f) => String -> Builder f
long "" = error "Cannot use an empty long-form option."
long s = Builder $ addLong (NE.fromList s)

short :: (HasShort f) => Char -> Builder f
short c = Builder $ addShort c

var :: (HasEnvVar f) => String -> Builder f
var v = Builder $ addEnvVar v
