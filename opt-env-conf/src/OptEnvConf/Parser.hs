{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Parser
  ( Functor (..),
    Applicative (..),
    Alternative (..),
    Selective (..),

    -- * Parser API
    setting,
    prefixed,
    subConfig,
    someNonEmpty,
    mapIO,
    withConfig,
    withYamlConfig,
    xdgYamlConfigFile,
    withLocalYamlConfig,
    enableDisableSwitch,
    choice,

    -- * Parser implementation
    Parser (..),
    Metavar,
    Help,
    showParserABit,
  )
where

import Autodocodec.Yaml
import Control.Applicative
import Control.Monad
import Control.Selective
import Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import OptEnvConf.ArgMap (Dashed (..))
import OptEnvConf.Reader
import OptEnvConf.Setting
import Path.IO
import System.FilePath

data Parser a where
  -- Functor
  ParserPure :: !a -> Parser a
  -- Applicative
  ParserFmap :: !(a -> b) -> !(Parser a) -> Parser b
  ParserAp :: !(Parser (a -> b)) -> !(Parser a) -> Parser b
  -- Selective
  ParserSelect :: !(Parser (Either a b)) -> !(Parser (a -> b)) -> Parser b
  -- Alternative
  ParserEmpty :: Parser a
  ParserAlt :: !(Parser a) -> !(Parser a) -> Parser a
  ParserMany :: !(Parser a) -> Parser [a]
  -- | Apply a computation to the result of a parser
  --
  -- This is intended for use-cases like resolving a file to an absolute path.
  -- It is morally ok for read-only IO actions but you will
  -- have a bad time if the action is not read-only.
  ParserMapIO :: !(a -> IO b) -> !(Parser a) -> Parser b
  -- | Load a configuration value and use it for the continuing parser
  ParserWithConfig :: Parser (Maybe JSON.Object) -> !(Parser a) -> Parser a
  -- | Prefixed env var
  ParserPrefixed :: !String -> !(Parser a) -> Parser a
  -- | Subconfig
  ParserSubconfig :: !String -> !(Parser a) -> Parser a
  -- | General settings
  ParserSetting :: !(Setting a) -> Parser a

instance Functor Parser where
  fmap f = \case
    ParserFmap g p -> ParserFmap (f . g) p
    ParserMapIO g p -> ParserMapIO (fmap f . g) p
    p -> ParserFmap f p

instance Applicative Parser where
  pure = ParserPure
  (<*>) = ParserAp

instance Selective Parser where
  select = ParserSelect

instance Alternative Parser where
  empty = ParserEmpty
  (<|>) p1 p2 =
    let isEmpty :: Parser a -> Bool
        isEmpty = \case
          ParserFmap _ p' -> isEmpty p'
          ParserPure _ -> False
          ParserAp pf pa -> isEmpty pf && isEmpty pa
          ParserSelect pe pf -> isEmpty pe && isEmpty pf
          ParserEmpty -> True
          ParserAlt _ _ -> False
          ParserMany _ -> False
          ParserMapIO _ p' -> isEmpty p'
          ParserWithConfig pc ps -> isEmpty pc && isEmpty ps
          ParserPrefixed _ p -> isEmpty p
          ParserSubconfig _ p -> isEmpty p
          ParserSetting _ -> False
     in case (isEmpty p1, isEmpty p2) of
          (True, True) -> ParserEmpty
          (True, False) -> p2
          (False, True) -> p1
          (False, False) -> ParserAlt p1 p2
  many = ParserMany

  some p = (:) <$> p <*> many p

showParserABit :: Parser a -> String
showParserABit = ($ "") . go 0
  where
    go :: Int -> Parser a -> ShowS
    go d = \case
      ParserFmap _ p ->
        showParen (d > 10) $
          showString "Fmap _ "
            . go 11 p
      ParserPure _ -> showParen (d > 10) $ showString "Pure _"
      ParserAp pf pa ->
        showParen (d > 10) $
          showString "Ap "
            . go 11 pf
            . showString " "
            . go 11 pa
      ParserSelect pe pf ->
        showParen (d > 10) $
          showString "Select "
            . go 11 pe
            . showString " "
            . go 11 pf
      ParserEmpty -> showString "Empty"
      ParserAlt p1 p2 ->
        showParen (d > 10) $
          showString "Alt "
            . go 11 p1
            . showString " "
            . go 11 p2
      ParserMany p ->
        showParen (d > 10) $
          showString "Many "
            . go 11 p
      ParserMapIO _ p ->
        showParen (d > 10) $
          showString "MapIO _ "
            . go 11 p
      ParserWithConfig p1 p2 ->
        showParen (d > 10) $
          showString "WithConfig _ "
            . go 11 p1
            . showString " "
            . go 11 p2
      ParserPrefixed prefix p ->
        showParen (d > 10) $
          showString "Prefixed "
            . showsPrec 11 prefix
            . showString " "
            . go 11 p
      ParserSubconfig key p ->
        showParen (d > 10) $
          showString "SubConfig "
            . showsPrec 11 key
            . showString " "
            . go 11 p
      ParserSetting p ->
        showParen (d > 10) $
          showString "Setting "
            . showSettingABit p

setting :: [Builder a] -> Parser a
setting = ParserSetting . buildSetting

buildSetting :: [Builder a] -> Setting a
buildSetting = completeBuilder . mconcat

prefixed :: String -> Parser a -> Parser a
prefixed = ParserPrefixed

subConfig :: String -> Parser a -> Parser a
subConfig = ParserSubconfig

someNonEmpty :: Parser a -> Parser (NonEmpty a)
someNonEmpty p = (:|) <$> p <*> many p

mapIO :: (a -> IO b) -> Parser a -> Parser b
mapIO = ParserMapIO

withConfig :: Parser (Maybe JSON.Object) -> Parser a -> Parser a
withConfig = ParserWithConfig

withYamlConfig :: Parser (Maybe FilePath) -> Parser a -> Parser a
withYamlConfig pathParser = withConfig $ mapIO (fmap join . mapM (resolveFile' >=> readYamlConfigFile)) pathParser

xdgYamlConfigFile :: FilePath -> Parser FilePath
xdgYamlConfigFile subdir =
  (\xdgDir -> xdgDir </> subdir </> "config.yaml")
    <$> setting
      [ reader str,
        env "XDG_CONFIG_HOME",
        metavar "DIRECTORY",
        help "Path to the XDG configuration directory"
      ]

withLocalYamlConfig :: Parser a -> Parser a
withLocalYamlConfig =
  withYamlConfig $
    Just
      <$> setting
        [ reader str,
          option,
          long "config-file",
          env "CONFIG_FILE",
          metavar "FILE",
          value "config.yaml",
          help "Path to the configuration file"
        ]

enableDisableSwitch :: Bool -> [Builder Bool] -> Parser Bool
enableDisableSwitch defaultBool builders =
  fromMaybe defaultBool <$> enableDisableSwitch' builders

enableDisableSwitch' :: [Builder Bool] -> Parser (Maybe Bool)
enableDisableSwitch' builders =
  optional $
    choice
      [ ParserSetting $
          modEnable $
            buildSetting builders,
        ParserSetting $
          modDisable $
            buildSetting builders,
        ParserSetting $
          modUnhidden $
            buildSetting $
              reader exists
                : builders
      ]
  where
    modEnable :: Setting Bool -> Setting Bool
    modEnable s =
      s
        { settingDasheds = mapMaybe (prefixDashedLong "enable-") (settingDasheds s),
          settingTryArgument = False,
          settingSwitchValue = Just True,
          settingTryOption = False,
          settingEnvVars = Nothing,
          settingConfigVals = Nothing,
          settingDefaultValue = Nothing,
          settingHidden = True
        }
    modDisable :: Setting Bool -> Setting Bool
    modDisable s =
      s
        { settingDasheds = mapMaybe (prefixDashedLong "disable-") (settingDasheds s),
          settingTryArgument = False,
          settingSwitchValue = Just False,
          settingTryOption = False,
          settingEnvVars = Nothing,
          settingConfigVals = Nothing,
          settingDefaultValue = Nothing,
          settingHidden = True
        }
    modUnhidden :: Setting Bool -> Setting Bool
    modUnhidden s =
      s
        { settingDasheds = mapMaybe (prefixDashedLong "(enable|disable)-") (settingDasheds s),
          settingSwitchValue = Just True, -- Unused
          settingMetavar = Just $ fromMaybe "ANY" $ settingMetavar s,
          settingDefaultValue = Nothing,
          settingHidden = False
        }
    prefixDashedLong :: String -> Dashed -> Maybe Dashed
    prefixDashedLong s = \case
      DashedShort _ -> Nothing
      DashedLong l -> Just $ DashedLong $ s `NE.prependList` l

choice :: [Parser a] -> Parser a
choice = \case
  [] -> ParserEmpty
  [c] -> c
  (c : cs) -> c <|> choice cs
