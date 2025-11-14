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
    filePathSetting,
    directoryPathSetting,
    strOption,
    strArgument,
    choice,
    mapIO,
    runIO,
    checkEither,
    checkMaybe,
    checkMapEither,
    checkMapIO,
    checkMapMaybe,
    checkMapEitherForgivable,
    checkMapIOForgivable,
    checkMapMaybeForgivable,
    allOrNothing,
    commands,
    command,
    defaultCommand,
    subArgs,
    subArgs_,
    subEnv,
    subEnv_,
    subConfig,
    subConfig_,
    subAll,
    subSettings,
    someNonEmpty,
    withDefault,
    withShownDefault,
    withConfig,
    withYamlConfig,
    withFirstYamlConfig,
    withCombinedYamlConfigs,
    withCombinedYamlConfigs',
    combineConfigObjects,
    xdgYamlConfigFile,
    withLocalYamlConfig,
    withConfigurableYamlConfig,
    withoutConfig,
    configuredConfigFile,
    enableDisableSwitch,
    yesNoSwitch,
    makeDoubleSwitch,
    readSecretTextFile,
    secretTextFileSetting,
    secretTextFileOrBareSetting,

    -- * Parser implementation
    Parser (..),
    Capability (..),
    HasParser (..),
    Command (..),
    CommandsBuilder (..),
    Metavar,
    Help,
    showParserABit,
    parserEraseSrcLocs,
    parserMapSetting,
    parserTraverseSetting,
    commandTraverseSetting,

    -- ** All or nothing implementation
    parserSettingsMap,

    -- ** Re-exports
    Functor (..),
    Applicative (..),
    Alternative (..),
    Selective (..),
  )
where

import Autodocodec.Yaml
import Control.Applicative
import Control.Monad
import Control.Selective
import Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack, withFrozenCallStack)
import OptEnvConf.Args (Dashed (..), prefixDashed)
import OptEnvConf.Casing
import OptEnvConf.Completer
import OptEnvConf.Reader
import OptEnvConf.Setting
import Path
import Path.IO
import Text.Show

data CommandsBuilder a
  = CommandsBuilderCommand !(Command a)
  | CommandsBuilderDefault !String

data Command a = Command
  { commandSrcLoc :: !(Maybe SrcLoc),
    commandArg :: !String,
    commandHelp :: !Help,
    commandParser :: !(Parser a)
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
-- The basic building block of a 'Parser' is a 'setting'.
-- 'setting's represent individual settings that you can then compose into larger parsers.
--
-- Much of the way you compose parsers happens via its type
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
  ParserAp ::
    !(Parser (a -> b)) ->
    !(Parser a) ->
    Parser b
  -- Selective
  ParserSelect ::
    !(Parser (Either a b)) ->
    !(Parser (a -> b)) ->
    Parser b
  -- Alternative
  ParserEmpty ::
    !(Maybe SrcLoc) ->
    Parser a
  ParserAlt ::
    !(Parser a) ->
    !(Parser a) ->
    Parser a
  ParserMany ::
    !(Maybe SrcLoc) ->
    !(Parser a) ->
    Parser [a]
  ParserSome ::
    !(Maybe SrcLoc) ->
    !(Parser a) ->
    Parser (NonEmpty a)
  ParserAllOrNothing ::
    !(Maybe SrcLoc) ->
    !(Parser a) ->
    Parser a
  -- Map, Check, and IO
  -- Prefer this over ParserCheckIO so that more can be checked when IO is turned off.
  ParserCheckPure ::
    !(Maybe SrcLoc) ->
    -- | Forgivable
    !Bool ->
    !(a -> Either String b) ->
    !(Parser a) ->
    Parser b
  -- Map, Check, and IO
  ParserCheckIO ::
    !(Maybe SrcLoc) ->
    -- | Forgivable
    !Bool ->
    !(a -> IO (Either String b)) ->
    !(Parser a) ->
    Parser b
  -- Annotate a parser with a required capability
  ParserRequireCapability ::
    !(Maybe SrcLoc) ->
    !Capability ->
    !(Parser a) ->
    Parser a
  -- Commands
  ParserCommands ::
    !(Maybe SrcLoc) ->
    -- Default command
    !(Maybe String) ->
    ![Command a] ->
    Parser a
  -- | Load a configuration value and use it for the continuing parser
  ParserWithConfig ::
    !(Maybe SrcLoc) ->
    !(Parser (Maybe JSON.Object)) ->
    !(Parser a) ->
    Parser a
  -- | General settings
  ParserSetting ::
    !(Maybe SrcLoc) ->
    !(Setting a) ->
    Parser a

data Capability = CapabilityAllowIO
  deriving (Show)

instance Functor Parser where
  -- We case-match to produce shallower parser structures.
  fmap f = \case
    ParserPure a -> ParserPure (f a)
    ParserAp pf pa -> ParserAp (fmap (fmap f) pf) pa
    ParserSelect pe pf -> ParserSelect (fmap (fmap f) pe) (fmap (fmap f) pf)
    ParserEmpty mLoc -> ParserEmpty mLoc
    ParserAlt p1 p2 -> ParserAlt (fmap f p1) (fmap f p2)
    ParserCheckPure mLoc forgivable g p -> ParserCheckPure mLoc forgivable (fmap f . g) p
    ParserCheckIO mLoc forgivable g p ->
      -- We can move the function into the IO, rather than wrapping with
      -- another ParserCheckPure, because the IO needs to happen first anyway.
      ParserCheckIO mLoc forgivable (fmap (fmap f) . g) p
    ParserRequireCapability mLoc cap p ->
      -- Don't move the function inside the capability, we don't want to run it
      -- if the capability is missing.
      ParserCheckPure Nothing True (Right . f) (ParserRequireCapability mLoc cap p)
    ParserCommands mLoc mDefault cs -> ParserCommands mLoc mDefault $ map (fmap f) cs
    ParserWithConfig mLoc pc pa -> ParserWithConfig mLoc pc (fmap f pa)
    -- TODO: make setting a functor and fmap here
    p -> ParserCheckPure Nothing True (Right . f) p

instance Applicative Parser where
  pure = ParserPure
  (<*>) p1 p2 = case (p1, p2) of
    -- Homomorphism law for applicative
    (ParserPure f, ParserPure a) -> ParserPure (f a)
    _ -> ParserAp p1 p2

instance Selective Parser where
  select = ParserSelect

instance Alternative Parser where
  empty = ParserEmpty Nothing
  (<|>) p1 p2 =
    let isEmpty :: Parser a -> Bool
        isEmpty = \case
          ParserPure _ -> False
          ParserAp pf pa -> isEmpty pf && isEmpty pa
          ParserSelect pe pf -> isEmpty pe && isEmpty pf
          ParserEmpty _ -> True
          ParserAlt _ _ -> False
          ParserMany _ p -> isEmpty p
          ParserSome _ p -> isEmpty p
          ParserAllOrNothing _ p -> isEmpty p
          ParserCheckPure _ _ _ p -> isEmpty p
          ParserCheckIO _ _ _ p -> isEmpty p
          ParserRequireCapability _ _ p -> isEmpty p
          ParserCommands _ _ cs -> null cs
          ParserWithConfig _ pc ps -> isEmpty pc && isEmpty ps
          ParserSetting _ _ -> False
     in case (isEmpty p1, isEmpty p2) of
          (True, True) -> ParserEmpty Nothing
          (True, False) -> p2
          (False, True) -> p1
          (False, False) ->
            let go p1' p2' = case (p1', p2') of
                  -- <|> needs to be associative, so we need to reorder the
                  -- alts to always be right-leaning
                  --
                  -- That means if we want to construct this parser, where p1 and p3 are commands parsers:
                  --    p
                  --   / \
                  -- p1   p2
                  --     /  \
                  --    p3   p4
                  --
                  -- We need to rearrange it to
                  --          p
                  --         / \
                  -- p1 ++ p3   p4
                  (ParserCommands _ _ _, ParserAlt p3' p4') ->
                    go (go p1' p3') p4'
                  (ParserCommands mLoc1 mDefault1 cs1, ParserCommands mLoc2 mDefault2 cs2) ->
                    ParserCommands (mLoc1 <|> mLoc2) (mDefault1 <|> mDefault2) (cs1 ++ cs2)
                  _ -> ParserAlt p1' p2'
             in go p1 p2
  many = ParserMany Nothing
  some = fmap NE.toList . ParserSome Nothing

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
      ParserEmpty mLoc ->
        showString "Empty "
          . showsPrec 11 mLoc
      ParserAlt p1 p2 ->
        showParen (d > 10) $
          showString "Alt "
            . go 11 p1
            . showString " "
            . go 11 p2
      ParserMany mLoc p ->
        showParen (d > 10) $
          showString "Many "
            . showsPrec 11 mLoc
            . showString " "
            . go 11 p
      ParserSome mLoc p ->
        showParen (d > 10) $
          showString "Some "
            . showsPrec 11 mLoc
            . showString " "
            . go 11 p
      ParserAllOrNothing mLoc p ->
        showParen (d > 10) $
          showString "AllOrNothing "
            . showsPrec 11 mLoc
            . showString " "
            . go 11 p
      ParserCheckPure mLoc forgivable _ p ->
        showParen (d > 10) $
          showString "CheckPure "
            . showsPrec 11 mLoc
            . showString " "
            . showsPrec 11 forgivable
            . showString " _ "
            . go 11 p
      ParserCheckIO mLoc forgivable _ p ->
        showParen (d > 10) $
          showString "CheckIO "
            . showsPrec 11 mLoc
            . showString " "
            . showsPrec 11 forgivable
            . showString " _ "
            . go 11 p
      ParserRequireCapability mLoc cap p ->
        showParen (d > 10) $
          showString "RequireCapability "
            . showsPrec 11 mLoc
            . showString " "
            . showsPrec 11 cap
            . showString " "
            . go 11 p
      ParserCommands mLoc mDefault cs ->
        showParen (d > 10) $
          showString "Commands "
            . showsPrec 11 mLoc
            . showString " "
            . showsPrec 11 mDefault
            . showString " "
            . showListWith
              showCommandABit
              cs
      ParserWithConfig mLoc p1 p2 ->
        showParen (d > 10) $
          showString "WithConfig _ "
            . showsPrec 11 mLoc
            . showString " "
            . go 11 p1
            . showString " "
            . go 11 p2
      ParserSetting mLoc p ->
        showParen (d > 10) $
          showString "Setting "
            . showsPrec 11 mLoc
            . showString " "
            . showSettingABit p

-- | A class of types that have a canonical settings parser.
--
-- There are no laws.
-- The closest rule to a law is that a user of an instance should not be surprised by its behaviour.
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

-- | A setting for @Path Abs File@.
--
-- This takes care of setting the 'reader' to 'str', setting the 'metavar' to @FILE_PATH@, autocompletion, and parsing the 'FilePath' into a @Path Abs File@.
filePathSetting ::
  (HasCallStack) =>
  [Builder FilePath] ->
  Parser (Path Abs File)
filePathSetting builders =
  mapIO resolveFile' $
    withFrozenCallStack $
      setting $
        [ reader str,
          metavar "FILE_PATH",
          completer filePath
        ]
          ++ builders

-- | A setting for @Path Abs dir@.
--
-- This takes care of setting the 'reader' to 'str', setting the 'metavar' to @DIRECTORY_PATH@, autocompletion, and parsing the 'FilePath' into a @Path Abs Dir@.
directoryPathSetting ::
  (HasCallStack) =>
  [Builder FilePath] ->
  Parser (Path Abs Dir)
directoryPathSetting builders =
  mapIO resolveDir' $
    withFrozenCallStack $
      setting $
        [ reader str,
          metavar "DIRECTORY_PATH",
          completer directoryPath
        ]
          ++ builders

-- | A 'setting' with 'option', a 'reader' set to 'str', and the 'metavar' set to @STR@.
--
-- Note that you can override the 'metavar' with another 'metavar' in the given list of builders.
--
-- This function may help with easier migration from @optparse-applicative@.
strOption :: (HasCallStack) => (IsString string) => [Builder string] -> Parser string
strOption builders =
  withFrozenCallStack $
    setting $
      option : reader str : metavar "STR" : builders

-- | A 'setting' with 'argument', a 'reader' set to 'str', and the 'metavar' set to @STR@.
--
-- Note that you can override the 'metavar' with another 'metavar' in the given list of builders.
--
-- This function may help with easier migration from @optparse-applicative@.
strArgument :: (HasCallStack) => (IsString string) => [Builder string] -> Parser string
strArgument builders =
  withFrozenCallStack $
    setting $
      argument : reader str : metavar "STR" : builders

-- | Like 'some' but with a more accurate type
someNonEmpty :: (HasCallStack) => Parser a -> Parser (NonEmpty a)
someNonEmpty = ParserSome mLoc
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Give a parser a default value.
--
-- This is morally equal to @(<|> pure a)@ but will give
-- you better documentation of the default value in many
-- cases.
--
-- This does nothing if the parser already has a default value.
withDefault :: (Show a) => a -> Parser a -> Parser a
withDefault = withShownDefault show

-- | Like 'withDefault' but lets you specfiy how to show the default value
-- yourself.
withShownDefault :: (a -> String) -> a -> Parser a -> Parser a
withShownDefault showDefault defaultValue = go
  where
    go p =
      let p' = p <|> pure defaultValue
       in case p of
            ParserPure a -> ParserPure a
            ParserAp {} -> p'
            ParserSelect {} -> p'
            ParserEmpty _ -> ParserPure defaultValue
            ParserAlt p1 p2 -> ParserAlt p1 (go p2)
            ParserMany {} -> p'
            ParserSome {} -> p'
            ParserAllOrNothing {} -> p'
            ParserCheckPure {} -> p'
            ParserCheckIO {} -> p'
            ParserRequireCapability mLoc cap p'' ->
              ParserRequireCapability mLoc cap (go p'')
            ParserCommands {} -> p'
            ParserWithConfig {} -> p'
            ParserSetting mLoc s -> case settingDefaultValue s of
              Nothing -> ParserSetting mLoc $ s {settingDefaultValue = Just (defaultValue, showDefault defaultValue)}
              Just _ -> p

-- | Try a list of parsers in order
choice :: (HasCallStack) => [Parser a] -> Parser a
choice = \case
  [] -> ParserEmpty mLoc
  [c] -> c
  (c : cs) -> c <|> choice cs
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Apply a computation to the result of a parser
--
-- This is intended for use-cases like resolving a file to an absolute path.
-- It is morally ok for read-only IO actions but you will
-- have a bad time if the action is not read-only.
mapIO :: (HasCallStack) => (a -> IO b) -> Parser a -> Parser b
mapIO func = withFrozenCallStack $ checkMapIO $ fmap Right . func

-- | Run an IO action without parsing anything
--
-- This action may be run more than once, so prefer to do IO outside of the parser.
runIO :: (HasCallStack) => IO a -> Parser a
runIO func = withFrozenCallStack $ mapIO (\() -> func) $ pure ()

-- | Like 'checkMapMaybe' but without changing the type
checkMaybe :: (HasCallStack) => (a -> Maybe a) -> Parser a -> Parser a
checkMaybe func p =
  withFrozenCallStack $
    checkMapMaybe func p

-- | Like 'checkMapEither' but without a helpful error message.
--
-- Prefer 'checkMapEither'.
checkMapMaybe :: (HasCallStack) => (a -> Maybe b) -> Parser a -> Parser b
checkMapMaybe func p =
  withFrozenCallStack $
    checkMapEither
      ( \a -> case func a of
          Nothing -> Left "checkMapMaybe failed without a helpful error message"
          Just b -> Right b
      )
      p

-- | Like 'checkMapEither' but without changing the type
checkEither :: (HasCallStack) => (a -> Either String b) -> Parser a -> Parser b
checkEither func p = withFrozenCallStack $ checkMapEither func p

-- | Check a 'Parser' after the fact, purely.
checkMapEither :: (HasCallStack) => (a -> Either String b) -> Parser a -> Parser b
checkMapEither = ParserCheckPure mLoc False
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Check a 'Parser' after the fact, allowing IO.
checkMapIO :: (HasCallStack) => (a -> IO (Either String b)) -> Parser a -> Parser b
checkMapIO = ParserCheckIO mLoc False
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Parse either all or none of the parser below.
--
-- If you don't use this function, and only some of the settings below are
-- defined, this parser will fail and the next alternative will be tried.
-- If you do use this function, this parser will error unforgivably if at least
-- one, but not all, of the settings below are defined.
--
-- If each setting has a corresponding forgivable error, consider this forgivable.
-- Consider all other forgivable errors unforgivable
--
-- For example, the following will parser will fail intsead of succeed when given the arguments below:
--
-- > ( choice
-- >     [ allOrNothing $
-- >         (,)
-- >           <$> setting [option, long "foo", reader auto, help "This one will exist", metavar "CHAR"]
-- >           <*> setting [option, long "bar", reader auto, help "This one will not exist", metavar "CHAR"],
-- >       pure ('a', 'b')
-- >     ]
-- > )
--
-- > ["--foo", "'a'"]
allOrNothing :: (HasCallStack) => Parser a -> Parser a
allOrNothing = ParserAllOrNothing mLoc
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Like 'checkMapMaybe', but allow trying the other side of any alternative if the result is Nothing.
checkMapMaybeForgivable :: (HasCallStack) => (a -> Maybe b) -> Parser a -> Parser b
checkMapMaybeForgivable func p =
  withFrozenCallStack $
    checkMapEitherForgivable
      ( \a -> case func a of
          Nothing -> Left "checkMapMaybeForgivable failed without a helpful error message"
          Just b -> Right b
      )
      p

-- | Like 'checkMapEither', but allow trying the other side of any alternative if the result is Nothing.
checkMapEitherForgivable :: (HasCallStack) => (a -> Either String b) -> Parser a -> Parser b
checkMapEitherForgivable = ParserCheckPure mLoc True
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Like 'checkMapIO', but allow trying the other side of any alternative if the result is Nothing.
checkMapIOForgivable :: (HasCallStack) => (a -> IO (Either String b)) -> Parser a -> Parser b
checkMapIOForgivable = ParserCheckIO mLoc True
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Declare multiple commands
--
-- Use 'command' to define a 'Command'.
commands :: (HasCallStack) => [CommandsBuilder a] -> Parser a
commands cbs =
  let (mDefault, cs) = go cbs
   in ParserCommands mLoc mDefault cs
  where
    go :: [CommandsBuilder a] -> (Maybe String, [Command a])
    go = \case
      [] -> (Nothing, [])
      (b : bs) ->
        let (mDefault, cs) = go bs
         in case b of
              CommandsBuilderCommand c -> (mDefault, c : cs)
              CommandsBuilderDefault d -> (mDefault <|> Just d, cs)
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Declare a single command with a name, documentation and parser
command ::
  (HasCallStack) =>
  -- | Name
  String ->
  -- | Documentation
  String ->
  -- | Parser
  Parser a ->
  CommandsBuilder a
command n docs parser = CommandsBuilderCommand $ Command mLoc n docs parser
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

defaultCommand ::
  -- | Name
  String ->
  CommandsBuilder a
defaultCommand = CommandsBuilderDefault

-- | Load a configuration value and use it for the given parser
withConfig :: (HasCallStack) => Parser (Maybe JSON.Object) -> Parser a -> Parser a
withConfig = ParserWithConfig mLoc
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)

-- | Don't load any configuration, but still shut up lint errors about 'conf'
-- being used without defining any way to load configuration.
--
-- This may be useful if you use a library's 'Parser' that uses 'conf' but do
-- not want to parse any configuration.
withoutConfig :: (HasCallStack) => Parser a -> Parser a
withoutConfig p = withFrozenCallStack $ withConfig (pure Nothing) p

-- | Load a YAML config file and use it for the given parser
withYamlConfig :: (HasCallStack) => Parser (Maybe (Path Abs File)) -> Parser a -> Parser a
withYamlConfig pathParser =
  withFrozenCallStack $
    withConfig $
      mapIO (fmap join . mapM readYamlConfigFile) pathParser

-- | Load the Yaml config in the first of the filepaths that points to something that exists.
withFirstYamlConfig :: (HasCallStack) => Parser [Path Abs File] -> Parser a -> Parser a
withFirstYamlConfig parsers =
  withFrozenCallStack $
    withConfig $
      mapIO readFirstYamlConfigFile $
        (<>) <$> (maybeToList <$> optional configuredConfigFile) <*> parsers

-- | Combine all Yaml config files that exist into a single combined config object.
withCombinedYamlConfigs :: Parser [Path Abs File] -> Parser a -> Parser a
withCombinedYamlConfigs = withCombinedYamlConfigs' combineConfigObjects

withCombinedYamlConfigs' :: (HasCallStack) => (Object -> JSON.Object -> JSON.Object) -> Parser [Path Abs File] -> Parser a -> Parser a
withCombinedYamlConfigs' combiner parsers =
  withFrozenCallStack $
    withConfig $
      mapIO (foldM resolveYamlConfigFile Nothing) $
        (<>) <$> (maybeToList <$> optional configuredConfigFile) <*> parsers
  where
    resolveYamlConfigFile :: Maybe JSON.Object -> Path Abs File -> IO (Maybe JSON.Object)
    resolveYamlConfigFile acc = fmap (combineMaybeObjects acc . join) . readYamlConfigFile
    -- left biased, first one wins
    combineMaybeObjects :: Maybe JSON.Object -> Maybe JSON.Object -> Maybe JSON.Object
    combineMaybeObjects Nothing mo = mo
    combineMaybeObjects mo Nothing = mo
    combineMaybeObjects (Just o1) (Just o2) = Just (combiner o1 o2)

combineConfigObjects :: JSON.Object -> JSON.Object -> JSON.Object
combineConfigObjects = KM.unionWith combineValues
  where
    combineValues :: Value -> Value -> Value
    combineValues (Object o) (Object o') = JSON.Object (combineConfigObjects o o')
    combineValues v _ = v

-- | Load @config.yaml@ from the given XDG configuration subdirectory
xdgYamlConfigFile :: (HasCallStack) => FilePath -> Parser (Path Abs File)
xdgYamlConfigFile subdir =
  mapIO
    ( \mXdgDir -> do
        xdgDir <- case mXdgDir of
          Just d -> resolveDir' d
          Nothing -> do
            home <- getHomeDir
            resolveDir home ".config"
        configDir <- resolveDir xdgDir subdir
        resolveFile configDir "config.yaml"
    )
    $ optional
    $ withFrozenCallStack
    $ setting
      [ help "Path to the XDG configuration directory",
        reader str,
        env "XDG_CONFIG_HOME",
        metavar "DIRECTORY",
        hidden
      ]

-- | Load a config file that is reconfigurable with an option and environment
-- variable but @config.yaml@ in the local working directory by default.
withLocalYamlConfig :: (HasCallStack) => Parser a -> Parser a
withLocalYamlConfig p =
  withFrozenCallStack $
    withConfigurableYamlConfig (mapIO resolveFile' (pure "config.yaml")) p

-- | Use the given 'Parser' for deciding which configuration file to load, but
-- only if 'configuredConfigFile' fails to define it first.
withConfigurableYamlConfig :: (HasCallStack) => Parser (Path Abs File) -> Parser a -> Parser a
withConfigurableYamlConfig pf pa =
  withFrozenCallStack $ withYamlConfig (Just <$> (configuredConfigFile <|> pf)) pa

-- | A standard parser for defining which configuration file to load.
--
-- This has no default value so you will have to combine it somehow.
configuredConfigFile :: (HasCallStack) => Parser (Path Abs File)
configuredConfigFile =
  filePathSetting
    [ option,
      long "config-file",
      env "CONFIG_FILE",
      help "Path to the configuration file"
    ]

-- | Define a setting for a 'Bool' with a given default value.
--
-- If you pass in 'long' values, it will have @--foobar@ and @--no-foobar@ switches.
-- If you pass in 'env' values, it will read those environment variables too.
-- If you pass in 'conf' values, it will read those configuration values too.
-- If you pass in a 'value' value, it will use that as the default value.
yesNoSwitch ::
  (HasCallStack) =>
  -- | Builders
  [Builder Bool] ->
  Parser Bool
yesNoSwitch builders =
  withFrozenCallStack $
    makeDoubleSwitch "" "no-" "[no-]" builders

-- | Define a setting for a 'Bool' with a given default value.
--
-- If you pass in 'long' values, it will have @--enable-foobar@ and @--disable-foobar@ switches.
-- If you pass in 'env' values, it will read those environment variables too.
-- If you pass in 'conf' values, it will read those configuration values too.
-- If you pass in a 'value' value, it will use that as the default value.
enableDisableSwitch ::
  (HasCallStack) =>
  -- | Builders
  [Builder Bool] ->
  Parser Bool
enableDisableSwitch builders =
  withFrozenCallStack $
    makeDoubleSwitch "enable-" "disable-" "(enable|disable)-" builders

makeDoubleSwitch ::
  (HasCallStack) =>
  -- | Prefix for 'True' 'long's
  String ->
  -- | Prefix for 'False' 'long's
  String ->
  -- | Prefix for the documented 'long's
  String ->
  -- | Builders
  [Builder Bool] ->
  Parser Bool
makeDoubleSwitch truePrefix falsePrefix helpPrefix builders =
  withFrozenCallStack $
    choice $
      catMaybes
        [ Just parseDummy,
          Just parseDisableSwitch,
          Just parseEnableSwitch,
          parseEnv,
          parseConfigVal,
          parseDefaultVal
        ]
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)
    s = buildSetting builders
    parseDefaultVal :: Maybe (Parser Bool)
    parseDefaultVal = do
      (dv, _) <- settingDefaultValue s
      pure $ pure dv

    parseEnableSwitch :: Parser Bool
    parseEnableSwitch =
      ParserSetting mLoc $
        Setting
          { settingDasheds = mapMaybe (prefixDashedLong truePrefix) (settingDasheds s),
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
            settingHelp = Nothing,
            settingCompleter = Nothing
          }
    parseDisableSwitch :: Parser Bool
    parseDisableSwitch =
      ParserSetting mLoc $
        Setting
          { settingDasheds = mapMaybe (prefixDashedLong falsePrefix) (settingDasheds s),
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
            settingHelp = Nothing,
            settingCompleter = Nothing
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
              settingHelp = settingHelp s,
              settingCompleter = Nothing
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
              settingHelp = settingHelp s,
              settingCompleter = Nothing
            }
    parseDummy :: Parser Bool
    parseDummy =
      ParserSetting mLoc $
        Setting
          { settingDasheds = mapMaybe (prefixDashedLong helpPrefix) (settingDasheds s),
            settingReaders = [],
            settingTryArgument = False,
            settingSwitchValue = Just True, -- Unused
            settingTryOption = False,
            settingEnvVars = Nothing,
            settingConfigVals = Nothing,
            settingDefaultValue = Nothing,
            settingExamples = [],
            settingHidden = False,
            settingMetavar = Nothing,
            settingHelp = settingHelp s,
            settingCompleter = Nothing
          }
    prefixDashedLong :: String -> Dashed -> Maybe Dashed
    prefixDashedLong prefix = \case
      DashedShort _ -> Nothing
      d -> Just $ prefixDashed prefix d

-- | Read a text file but strip whitespace so it can be edited with an editor
-- that messes with line endings.
readSecretTextFile :: Path Abs File -> IO Text
readSecretTextFile = fmap T.strip . T.readFile . fromAbsFile

-- | Load a secret from a text file, with 'readSecretTextFile'
secretTextFileSetting :: (HasCallStack) => [Builder FilePath] -> Parser Text
secretTextFileSetting bs = withFrozenCallStack $ mapIO readSecretTextFile $ filePathSetting bs

-- | Load a secret from a text file, with 'readSecretTextFile', or specify it
-- directly
secretTextFileOrBareSetting :: (HasCallStack) => [Builder FilePath] -> Parser Text
secretTextFileOrBareSetting bs =
  withFrozenCallStack $
    choice $
      catMaybes
        [ bareOption,
          fileOption,
          bareEnv,
          fileEnv,
          bareConf,
          fileConf
        ]
  where
    mLoc = snd <$> listToMaybe (getCallStack callStack)
    b = mconcat $ bs ++ [reader str]
    bareSetting p f = do
      let s = completeBuilder $ mconcat [mapMaybeBuilder f b, reader str, metavar "SECRET"]
      guard $ p s
      pure $ T.pack <$> ParserSetting mLoc s
    fileSetting p f = do
      let s = completeBuilder $ mconcat [mapMaybeBuilder f b, reader str, metavar "FILE_PATH"]
      guard $ p s
      pure $ mapIO (resolveFile' >=> readSecretTextFile) $ ParserSetting mLoc s

    bareOption = bareSetting settingTryOption $ \case
      BuildTryArgument -> Nothing
      BuildAddShort s -> Just $ BuildAddShort s
      BuildAddLong l -> Just $ BuildAddLong l
      BuildAddEnv _ -> Nothing
      BuildAddConf _ -> Nothing
      BuildSetDefault _ _ -> Nothing
      i -> Just i
    fileOption = fileSetting settingTryOption $ \case
      BuildTryArgument -> Nothing
      BuildAddShort _ -> Nothing
      BuildAddLong l -> Just $ BuildAddLong (l <> NE.fromList "-file")
      BuildAddEnv _ -> Nothing
      BuildAddConf _ -> Nothing
      BuildSetDefault _ _ -> Nothing
      i -> Just i
    bareEnv = bareSetting (isJust . settingEnvVars) $ \case
      BuildTryArgument -> Nothing
      BuildTryOption -> Nothing
      BuildAddShort _ -> Nothing
      BuildAddLong _ -> Nothing
      BuildAddEnv v -> Just $ BuildAddEnv v
      BuildAddConf _ -> Nothing
      BuildSetDefault _ _ -> Nothing
      i -> Just i
    fileEnv = fileSetting (isJust . settingEnvVars) $ \case
      BuildTryArgument -> Nothing
      BuildTryOption -> Nothing
      BuildAddShort _ -> Nothing
      BuildAddLong _ -> Nothing
      BuildAddEnv e ->
        Just $
          BuildAddEnv $
            suffixEnvVarSetting "_FILE" e
      BuildAddConf _ -> Nothing
      BuildSetDefault _ _ -> Nothing
      i -> Just i
    bareConf = bareSetting (isJust . settingConfigVals) $ \case
      BuildTryArgument -> Nothing
      BuildTryOption -> Nothing
      BuildAddShort _ -> Nothing
      BuildAddLong _ -> Nothing
      BuildAddEnv _ -> Nothing
      BuildAddConf k -> Just $ BuildAddConf k
      BuildSetDefault _ _ -> Nothing
      i -> Just i
    fileConf = fileSetting (isJust . settingConfigVals) $ \case
      BuildTryArgument -> Nothing
      BuildTryOption -> Nothing
      BuildAddShort _ -> Nothing
      BuildAddLong _ -> Nothing
      BuildAddEnv _ -> Nothing
      BuildAddConf k -> Just $ BuildAddConf $ suffixConfigValSettingKey "-file" k
      BuildSetDefault _ _ -> Nothing
      i -> Just i

-- | Prefix all 'long's and 'short's with a given 'String'.
{-# ANN subArgs ("NOCOVER" :: String) #-}
subArgs :: String -> Parser a -> Parser a
subArgs prefix = parserMapSetting $ \s ->
  s {settingDasheds = map (prefixDashed prefix) (settingDasheds s)}

-- | Helper function for calling 'subArgs' with 'toArgCase' and a @'-'@ appended.
--
-- > subArgs_ s = subArgs (toArgCase s <> "-")
subArgs_ :: String -> Parser a -> Parser a
subArgs_ s = subArgs (toArgCase s <> "-")

-- | Prefix all 'env's with a given 'String'.
{-# ANN subEnv ("NOCOVER" :: String) #-}
subEnv :: String -> Parser a -> Parser a
subEnv prefix = parserMapSetting $ \s ->
  s {settingEnvVars = NE.map (prefixEnvVarSetting prefix) <$> settingEnvVars s}

-- | Helper function for calling 'subEnv' with 'toEnvCase' and a @'_'@ appended.
--
-- > subEnv_ s = subEnv (toEnvCase s <> "_")
subEnv_ :: String -> Parser a -> Parser a
subEnv_ s = subEnv (toEnvCase s <> "_")

-- | Prefix all 'conf's with a given 'String'.
{-# ANN subConfig ("NOCOVER" :: String) #-}
subConfig :: String -> Parser a -> Parser a
subConfig prefix = parserMapSetting $ \s ->
  s {settingConfigVals = NE.map (prefixConfigValSetting prefix) <$> settingConfigVals s}

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

-- | Use the 'settingsParser' of a given type, but prefixed with a 'subAll' and 'allOrNothing'.
--
-- > subSettings prefix = allOrNothing $ subAll prefix settingsParser
subSettings :: (HasCallStack) => (HasParser a) => String -> Parser a
subSettings prefix = withFrozenCallStack allOrNothing $ subAll prefix settingsParser

-- | Erase all source locations in a parser.
--
-- This may be useful when golden-testing the shown parser.
{-# ANN parserEraseSrcLocs ("NOCOVER" :: String) #-}
parserEraseSrcLocs :: Parser a -> Parser a
parserEraseSrcLocs = go
  where
    go :: forall q. Parser q -> Parser q
    go = \case
      ParserPure a -> ParserPure a
      ParserAp p1 p2 -> ParserAp (go p1) (go p2)
      ParserSelect p1 p2 -> ParserSelect (go p1) (go p2)
      ParserEmpty _ -> ParserEmpty Nothing
      ParserAlt p1 p2 -> ParserAlt (go p1) (go p2)
      ParserMany _ p -> ParserMany Nothing (go p)
      ParserSome _ p -> ParserSome Nothing (go p)
      ParserAllOrNothing _ p -> ParserAllOrNothing Nothing (go p)
      ParserCheckPure _ forgivable f p -> ParserCheckPure Nothing forgivable f (go p)
      ParserCheckIO _ forgivable f p -> ParserCheckIO Nothing forgivable f (go p)
      ParserRequireCapability _ cap p -> ParserRequireCapability Nothing cap (go p)
      ParserCommands _ mDefault cs -> ParserCommands Nothing mDefault $ map commandEraseSrcLocs cs
      ParserWithConfig _ p1 p2 -> ParserWithConfig Nothing (go p1) (go p2)
      ParserSetting _ s -> ParserSetting Nothing s

commandEraseSrcLocs :: Command a -> Command a
commandEraseSrcLocs c =
  c
    { commandSrcLoc = Nothing,
      commandParser = parserEraseSrcLocs (commandParser c)
    }

-- | Map all 'Setting' in a 'Parser'.
{-# ANN parserMapSetting ("NOCOVER" :: String) #-}
parserMapSetting :: (forall a. Setting a -> Setting a) -> Parser s -> Parser s
parserMapSetting func = runIdentity . parserTraverseSetting (Identity . func)

-- | Traverse all 'Setting's in a 'Parser'.
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
      ParserEmpty mLoc -> pure $ ParserEmpty mLoc
      ParserAlt p1 p2 -> ParserAlt <$> go p1 <*> go p2
      ParserMany mLoc p -> ParserMany mLoc <$> go p
      ParserSome mLoc p -> ParserSome mLoc <$> go p
      ParserAllOrNothing mLoc p -> ParserAllOrNothing mLoc <$> go p
      ParserCheckPure mLoc forgivable f p -> ParserCheckPure mLoc forgivable f <$> go p
      ParserCheckIO mLoc forgivable f p -> ParserCheckIO mLoc forgivable f <$> go p
      ParserRequireCapability mLoc cap p -> ParserRequireCapability mLoc cap <$> go p
      ParserCommands mLoc mDefault cs -> ParserCommands mLoc mDefault <$> traverse (commandTraverseSetting func) cs
      ParserWithConfig mLoc p1 p2 -> ParserWithConfig mLoc <$> go p1 <*> go p2
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

parserSettingsMap :: Parser a -> Map SettingHash SrcLoc
parserSettingsMap = go
  where
    go :: Parser a -> Map SettingHash SrcLoc
    go = \case
      ParserPure _ -> M.empty
      ParserAp p1 p2 -> M.union (go p1) (go p2)
      ParserSelect p1 p2 -> M.union (go p1) (go p2)
      ParserEmpty _ -> M.empty
      ParserAlt p1 p2 -> M.union (go p1) (go p2)
      ParserMany _ p -> go p
      ParserSome _ p -> go p
      ParserAllOrNothing _ p -> go p -- TODO is this right?
      ParserCheckPure _ _ _ p -> go p
      ParserCheckIO _ _ _ p -> go p
      ParserRequireCapability _ _ p -> go p
      ParserCommands _ _ cs -> M.unions $ map (go . commandParser) cs
      ParserWithConfig _ p1 p2 -> M.union (go p1) (go p2)
      -- The nothing part shouldn't happen but I don't know when it doesn't
      ParserSetting mLoc s -> maybe M.empty (M.singleton (hashSetting s)) mLoc
