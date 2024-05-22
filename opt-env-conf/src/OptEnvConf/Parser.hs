{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.Parser
  ( -- * Parser API
    setting,
    choice,
    mapIO,
    checkMap,
    checkMapIO,
    commands,
    command,
    subArgs,
    subArgs_,
    subEnv,
    subEnv_,
    subConfig,
    subConfig_,
    subAll,
    subSettings,
    someNonEmpty,
    withConfig,
    withYamlConfig,
    xdgYamlConfigFile,
    withLocalYamlConfig,
    enableDisableSwitch,
    readTextSecretFile,

    -- * Parser implementation
    Parser (..),
    HasParser (..),
    Command (..),
    Metavar,
    Help,
    showParserABit,
    parserMapSetting,
    parserTraverseSetting,

    -- ** Re-exports
    Functor (..),
    Applicative (..),
    Alternative (..),
    Selective (..),
  )
where

import Autodocodec.Yaml
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Selective
import Data.Aeson as JSON
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack)
import OptEnvConf.Args (Dashed (..), prefixDashed)
import OptEnvConf.Casing
import OptEnvConf.Reader
import OptEnvConf.Setting
import Path.IO
import System.FilePath
import Text.Show

data Command a = Command
  { commandArg :: String,
    commandHelp :: Help,
    commandParser :: Parser a
  }

instance Functor Command where
  fmap f c = c {commandParser = fmap f (commandParser c)}

showCommandABit :: Command a -> ShowS
showCommandABit Command {..} =
  showString "Command "
    . showsPrec 11 commandArg
    . showString " "
    . showsPrec 11 commandHelp
    . showString " "
    . showParserPrec 11 commandParser

-- | A 'Parser' structure
--
-- A @Parser a@ value represents each of these all at once:
--
--     * A way to run it to parse an @a@
--     * A way to document it in various ways
--     * A way to run it to perform shell completion
--
-- Much of the way you build parsers happens via its type
-- class instances.
-- In particular:
--
--     * '<$>' from 'Functor' to map over 'Parser's
--     * '<*>' from 'Applicative' to "and" 'Parser's
--     * '<|>' from 'Alternative' to "or" 'Parser's
--     * 'optional' from 'Alternative' to optionally run a parser
--     * 'many' and 'some' from 'Alternative' to run the same parser multiple times.
--
-- You can run a parser with 'runParser', or give your type an instance of
-- 'HasParser' and run the parser with 'runSettingsParser'.
data Parser a where
  -- Functor
  ParserPure :: !a -> Parser a
  -- Applicative
  ParserAp :: !(Parser (a -> b)) -> !(Parser a) -> Parser b
  -- Selective
  ParserSelect :: !(Parser (Either a b)) -> !(Parser (a -> b)) -> Parser b
  -- Alternative
  ParserEmpty :: Parser a
  ParserAlt :: !(Parser a) -> !(Parser a) -> Parser a
  ParserMany :: !(Parser a) -> Parser [a]
  -- Map, Check, and IO
  ParserCheck :: (a -> IO (Either String b)) -> Parser a -> Parser b
  -- Commands
  ParserCommands :: [Command a] -> Parser a
  -- | Load a configuration value and use it for the continuing parser
  ParserWithConfig :: Parser (Maybe JSON.Object) -> !(Parser a) -> Parser a
  -- | General settings
  ParserSetting :: !(Maybe SrcLoc) -> !(Setting a) -> Parser a

instance Functor Parser where
  -- We case-match to produce shallower parser structures.
  fmap f = \case
    ParserPure a -> ParserPure (f a)
    ParserAp pf pa -> ParserAp (fmap (fmap f) pf) pa
    ParserSelect pe pf -> ParserSelect (fmap (fmap f) pe) (fmap (fmap f) pf)
    ParserEmpty -> ParserEmpty
    ParserAlt p1 p2 -> ParserAlt (fmap f p1) (fmap f p2)
    ParserCheck g p -> ParserCheck (fmap (fmap f) . g) p
    ParserCommands cs -> ParserCommands $ map (fmap f) cs
    ParserWithConfig pc pa -> ParserWithConfig pc (fmap f pa)
    -- TODO: make setting a functor and fmap here
    p -> ParserCheck (pure . Right . f) p

instance Applicative Parser where
  pure = ParserPure
  (<*>) p1 p2 = case (p1, p2) of
    -- Homomorphism law for applicative
    (ParserPure f, ParserPure a) -> ParserPure (f a)
    _ -> ParserAp p1 p2

instance Selective Parser where
  select = ParserSelect

instance Alternative Parser where
  empty = ParserEmpty
  (<|>) p1 p2 =
    let isEmpty :: Parser a -> Bool
        isEmpty = \case
          ParserPure _ -> False
          ParserAp pf pa -> isEmpty pf && isEmpty pa
          ParserSelect pe pf -> isEmpty pe && isEmpty pf
          ParserEmpty -> True
          ParserAlt _ _ -> False
          ParserMany _ -> False
          ParserCheck _ p -> isEmpty p
          ParserCommands cs -> null cs
          ParserWithConfig pc ps -> isEmpty pc && isEmpty ps
          ParserSetting _ _ -> False
     in case (isEmpty p1, isEmpty p2) of
          (True, True) -> ParserEmpty
          (True, False) -> p2
          (False, True) -> p1
          (False, False) -> ParserAlt p1 p2
  many = ParserMany

  some p = (:) <$> p <*> many p

showParserABit :: Parser a -> String
showParserABit = ($ "") . showParserPrec 0

showParserPrec :: Int -> Parser a -> ShowS
showParserPrec = go
  where
    go :: Int -> Parser a -> ShowS
    go d = \case
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
      ParserCheck _ p ->
        showParen (d > 10) $
          showString "Check _ "
            . go 11 p
      ParserCommands cs ->
        showParen (d > 10) $
          showString "Commands "
            . showListWith
              showCommandABit
              cs
      ParserWithConfig p1 p2 ->
        showParen (d > 10) $
          showString "WithConfig _ "
            . go 11 p1
            . showString " "
            . go 11 p2
      ParserSetting srcLoc p ->
        showParen (d > 10) $
          showString "Setting "
            . showsPrec 11 srcLoc
            . showString " "
            . showSettingABit p

-- | A class of types that have a canonical settings parser.
class HasParser a where
  settingsParser :: Parser a

-- | 'setting's are the building blocks of 'Parser's.
--
-- 'setting' lets you put together different builders to define what to parse.
--
-- Here are some common examples:
--
--     * Argument
--
--         @
--         setting
--            [ help "Document your argument"
--            , reader str -- The argument is a string
--            , argument
--            ] :: Parser String
--         @
--
--     * Switch
--
--         @
--         setting
--            [ help "Document your switch"
--            , switch True -- The value of the switch when activated
--            , long 'foo' -- "--foo"
--            , short 'f' -- "-f"
--            , value False -- The default value of the switch
--            ] :: Parser Bool
--         @
--
--     * Option
--
--         @
--         setting
--            [ help "Document your option"
--            , reader str -- The argument is a string
--            , long 'foo' -- "--foo"
--            , short 'f' -- "-f"
--            , option
--            ] :: Parser String
--         @
--
--     * Environment Variable
--
--         @
--         setting
--            [ help "Document your environment variable"
--            , reader str -- The argument is a string
--            , env "FOO_BAR"
--            ] :: Parser String
--         @
--
--     * Configuration Value
--
--         @
--         setting
--            [ help "Document your configuration value"
--            , conf "foo-bar"
--            ] :: Parser String
--         @
--
--     * Some combination
--
--         @
--         setting
--            [ help "Document your configuration value"
--            , conf "foo-bar"
--            ] :: Parser String
--         @
--
--         Note that parsing is always tried in this order when using a combined setting:
--
--         1. Argument
--         2. Switch
--         3. Option
--         4. Environment variable
--         5. Configuration value
--
--         (Hence the name of the package.)
setting :: (HasCallStack) => [Builder a] -> Parser a
setting = ParserSetting mLoc . buildSetting
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

buildSetting :: [Builder a] -> Setting a
buildSetting = completeBuilder . mconcat

-- | Like 'some' but with a more accurate type
someNonEmpty :: Parser a -> Parser (NonEmpty a)
someNonEmpty p = (:|) <$> p <*> many p

-- | Try a list of parsers in order
choice :: [Parser a] -> Parser a
choice = \case
  [] -> ParserEmpty
  [c] -> c
  (c : cs) -> c <|> choice cs

-- | Apply a computation to the result of a parser
--
-- This is intended for use-cases like resolving a file to an absolute path.
-- It is morally ok for read-only IO actions but you will
-- have a bad time if the action is not read-only.
mapIO :: (a -> IO b) -> Parser a -> Parser b
mapIO func = checkMapIO $ fmap Right . func

checkMap :: (a -> Either String b) -> Parser a -> Parser b
checkMap func = checkMapIO (pure . func)

checkMapIO :: (a -> IO (Either String b)) -> Parser a -> Parser b
checkMapIO = ParserCheck

-- | Declare multiple commands
commands :: [Command a] -> Parser a
commands = ParserCommands

-- | Declare a single command with a name, documentation and parser
command :: String -> String -> Parser a -> Command a
command = Command

-- | Load a configuration value and use it for the given parser
withConfig :: Parser (Maybe JSON.Object) -> Parser a -> Parser a
withConfig = ParserWithConfig

-- | Load a YAML config file and use it for the given parser
withYamlConfig :: Parser (Maybe FilePath) -> Parser a -> Parser a
withYamlConfig pathParser = withConfig $ mapIO (fmap join . mapM (resolveFile' >=> readYamlConfigFile)) pathParser

-- | Load @config.yaml@ from the given XDG configuration subdirectory
xdgYamlConfigFile :: FilePath -> Parser FilePath
xdgYamlConfigFile subdir =
  (\xdgDir -> xdgDir </> subdir </> "config.yaml")
    <$> setting
      [ reader str,
        env "XDG_CONFIG_HOME",
        metavar "DIRECTORY",
        help "Path to the XDG configuration directory"
      ]

-- | Load a config file that is reconfigurable with an option and environment
-- variable but @config.yaml@ in the local working directory by default.
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

-- | Define a setting for a 'Bool' with a given default value.
--
-- If you pass in `long` values, it will have `--enable-foobar` and `--disable-foobar` switches.
-- If you pass in `env` values, it will read an environment variable too.
-- If you pass in `conf` values, it will read a configuration value too.
enableDisableSwitch :: (HasCallStack) => Bool -> [Builder Bool] -> Parser Bool
enableDisableSwitch defaultBool builders =
  choice $
    catMaybes
      [ Just parseDummy,
        Just parseDisableSwitch,
        Just parseEnableSwitch,
        parseEnv,
        parseConfigVal,
        Just $ pure defaultBool
      ]
  where
    s = buildSetting builders
    mLoc = snd <$> listToMaybe (getCallStack callStack)
    parseEnableSwitch :: Parser Bool
    parseEnableSwitch =
      ParserSetting mLoc $
        Setting
          { settingDasheds = mapMaybe (prefixDashedLong "enable-") (settingDasheds s),
            settingReaders = [],
            settingTryArgument = False,
            settingSwitchValue = Just True,
            settingTryOption = False,
            settingEnvVars = Nothing,
            settingConfigVals = Nothing,
            settingDefaultValue = Nothing,
            settingExamples = [],
            settingHidden = True,
            settingMetavar = Nothing,
            settingHelp = Nothing
          }
    parseDisableSwitch :: Parser Bool
    parseDisableSwitch =
      ParserSetting mLoc $
        Setting
          { settingDasheds = mapMaybe (prefixDashedLong "disable-") (settingDasheds s),
            settingReaders = [],
            settingTryArgument = False,
            settingSwitchValue = Just False,
            settingTryOption = False,
            settingEnvVars = Nothing,
            settingConfigVals = Nothing,
            settingDefaultValue = Nothing,
            settingExamples = [],
            settingHidden = True,
            settingMetavar = Nothing,
            settingHelp = Nothing
          }

    parseEnv :: Maybe (Parser Bool)
    parseEnv = do
      ne <- settingEnvVars s
      pure $
        ParserSetting mLoc $
          Setting
            { settingDasheds = [],
              settingReaders = (auto :: Reader Bool) : settingReaders s,
              settingTryArgument = False,
              settingSwitchValue = Nothing,
              settingTryOption = False,
              settingEnvVars = Just ne,
              settingConfigVals = Nothing,
              settingDefaultValue = Nothing,
              settingExamples = [],
              settingHidden = False,
              settingMetavar = Just "BOOL",
              settingHelp = settingHelp s
            }
    parseConfigVal :: Maybe (Parser Bool)
    parseConfigVal = do
      ne <- settingConfigVals s
      pure $
        ParserSetting mLoc $
          Setting
            { settingDasheds = [],
              settingReaders = [],
              settingTryArgument = False,
              settingSwitchValue = Nothing,
              settingTryOption = False,
              settingEnvVars = Nothing,
              settingConfigVals = Just ne,
              settingDefaultValue = Nothing,
              settingExamples = [],
              settingHidden = False,
              settingMetavar = Nothing,
              settingHelp = settingHelp s
            }
    parseDummy :: Parser Bool
    parseDummy =
      ParserSetting mLoc $
        Setting
          { settingDasheds = mapMaybe (prefixDashedLong "(enable|disable)-") (settingDasheds s),
            settingReaders = [],
            settingTryArgument = False,
            settingSwitchValue = Just defaultBool, -- Unused
            settingTryOption = False,
            settingEnvVars = Nothing,
            settingConfigVals = Nothing,
            settingDefaultValue = Nothing,
            settingExamples = [],
            settingHidden = False,
            settingMetavar = Nothing,
            settingHelp = settingHelp s
          }
    prefixDashedLong :: String -> Dashed -> Maybe Dashed
    prefixDashedLong prefix = \case
      DashedShort _ -> Nothing
      d -> Just $ prefixDashed prefix d

-- | Read a text file but strip whitespace so it can be edited with an editor
-- that messes with line endings.
readTextSecretFile :: FilePath -> IO Text
readTextSecretFile = fmap T.strip . T.readFile

{-# ANN subArgs ("NOCOVER" :: String) #-}
subArgs :: String -> Parser a -> Parser a
subArgs prefix = parserMapSetting $ \s ->
  s {settingDasheds = map (prefixDashed prefix) (settingDasheds s)}

-- | Helper function for calling 'subArgs' with 'toArgCase' and a '-' appended.
--
-- > subArgs_ s = subArgs (toArgCase s <> "-")
subArgs_ :: String -> Parser a -> Parser a
subArgs_ s = subArgs (toArgCase s <> "-")

{-# ANN subEnv ("NOCOVER" :: String) #-}
subEnv :: String -> Parser a -> Parser a
subEnv prefix = parserMapSetting $ \s ->
  s {settingEnvVars = NE.map (prefix <>) <$> settingEnvVars s}

-- | Helper function for calling 'subEnv' with 'toEnvCase' and a '_' appended.
--
-- > subEnv_ s = subEnv (toEnvCase s <> "_")
subEnv_ :: String -> Parser a -> Parser a
subEnv_ s = subEnv (toEnvCase s <> "_")

{-# ANN subConfig ("NOCOVER" :: String) #-}
subConfig :: String -> Parser a -> Parser a
subConfig prefix = parserMapSetting $ \s ->
  s {settingConfigVals = NE.map (first (prefix <|)) <$> settingConfigVals s}

-- | Helper function for calling 'subConfig' with 'toConfigCase'.
--
-- > subConfig_ s = subConfig (toConfigCase s)
subConfig_ :: String -> Parser a -> Parser a
subConfig_ s = subConfig (toConfigCase s)

-- | Helper function for calling 'subArgs_', 'subEnv_' and 'subConfig_' with
-- the same prefix.
--
-- > subAll = subArgs_ prefix . subEnv_ prefix . subConfig_ prefix
subAll :: String -> Parser a -> Parser a
subAll prefix =
  subArgs_ prefix
    . subEnv_ prefix
    . subConfig_ prefix

subSettings :: (HasParser a) => String -> Parser a
subSettings prefix = subAll prefix settingsParser

{-# ANN parserMapSetting ("NOCOVER" :: String) #-}
parserMapSetting :: (forall a. Setting a -> Setting a) -> Parser s -> Parser s
parserMapSetting func = runIdentity . parserTraverseSetting (Identity . func)

{-# ANN parserTraverseSetting ("NOCOVER" :: String) #-}
parserTraverseSetting ::
  forall f s.
  (Applicative f) =>
  (forall a. Setting a -> f (Setting a)) ->
  Parser s ->
  f (Parser s)
parserTraverseSetting func = go
  where
    go :: forall q. Parser q -> f (Parser q)
    go = \case
      ParserPure a -> pure $ ParserPure a
      ParserAp p1 p2 -> ParserAp <$> go p1 <*> go p2
      ParserSelect p1 p2 -> ParserSelect <$> go p1 <*> go p2
      ParserEmpty -> pure ParserEmpty
      ParserAlt p1 p2 -> ParserAlt <$> go p1 <*> go p2
      ParserMany p -> ParserMany <$> go p
      ParserCheck f p -> ParserCheck f <$> go p
      ParserCommands cs -> ParserCommands <$> traverse (commandTraverseSetting func) cs
      ParserWithConfig p1 p2 -> ParserWithConfig <$> go p1 <*> go p2
      ParserSetting mLoc s -> ParserSetting mLoc <$> func s

{-# ANN commandTraverseSetting ("NOCOVER" :: String) #-}
commandTraverseSetting ::
  forall f s.
  (Applicative f) =>
  (forall a. Setting a -> f (Setting a)) ->
  Command s ->
  f (Command s)
commandTraverseSetting func c = do
  (\p -> c {commandParser = p})
    <$> parserTraverseSetting func (commandParser c)
