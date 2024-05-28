{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OptEnvConf.Opt where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (Dashed (..))

data OptionGenerals f = OptionGenerals
  -- TODO Completer
  { optionGeneralSpecifics :: f,
    optionGeneralHelp :: !(Maybe String)
  }

newtype Builder f = Builder {unBuilder :: OptionGenerals f -> OptionGenerals f}

instance Semigroup (Builder f) where
  (<>) (Builder f1) (Builder f2) = Builder (f2 . f1)

class HasReader r a where
  setReader :: Reader r -> a -> a

instance HasReader a f => HasReader a (OptionGenerals f) where
  setReader r op = op {optionGeneralSpecifics = setReader r (optionGeneralSpecifics op)}

class HasLong a where
  addLong :: NonEmpty Char -> a -> a

class HasShort a where
  addShort :: Char -> a -> a

-- data SwitchSpecifics = SwitchSpecifics
--
-- type SwitchParser = OptionGenerals SwitchSpecifics
--
-- type SwitchBuilder a = SwitchParser a -> SwitchParser a

data OptionSpecifics a = OptionSpecifics
  -- TODO completer
  { optionSpecificsReader :: !(Maybe (Reader a)),
    optionSpecificsDasheds :: ![Dashed]
  }

instance HasReader a (OptionSpecifics a) where
  setReader r os = os {optionSpecificsReader = Just r}

instance HasLong (OptionSpecifics a) where
  addLong s os = os {optionSpecificsDasheds = DashedLong s : optionSpecificsDasheds os}

instance HasShort (OptionSpecifics a) where
  addShort c os = os {optionSpecificsDasheds = DashedShort c : optionSpecificsDasheds os}

type OptionParser a = OptionGenerals (OptionSpecifics a)

type OptionBuilder a = Builder (OptionParser a)

data ArgumentSpecifics a = ArgumentSpecifics
  -- TODO Completer
  { argumentSpecificsReader :: !(Maybe (Reader a))
  }

instance HasReader a (ArgumentSpecifics a) where
  setReader r os = os {argumentSpecificsReader = Just r}

type ArgumentParser a = OptionGenerals (ArgumentSpecifics a)

type ArgumentBuilder a = Builder (ArgumentParser a)

type Reader a = String -> Either String a

str :: Reader String
str = Right

reader :: HasReader a f => Reader a -> Builder f
reader r = Builder $ setReader r

help :: String -> Builder f
help s = Builder $ \op -> op {optionGeneralHelp = Just s}

long :: HasLong f => String -> Builder f
long "" = error "Cannot use an empty long-form option."
long s = Builder $ \op -> op {optionGeneralSpecifics = addLong (NE.fromList s) (optionGeneralSpecifics op)}

short :: HasShort f => Char -> Builder f
short c = Builder $ \op -> op {optionGeneralSpecifics = addShort c (optionGeneralSpecifics op)}
