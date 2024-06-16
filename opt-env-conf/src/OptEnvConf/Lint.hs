{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Lint
  ( LintError (..),
    renderLintErrors,
    renderLintError,
    lintParserTest,
    lintParser,
  )
where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (SrcLoc, prettySrcLoc)
import OptEnvConf.Parser
import OptEnvConf.Setting
import OptEnvConf.Validation
import System.Exit
import System.IO (stderr)
import Text.Colour
import Text.Colour.Capabilities.FromEnv

data LintError = LintError
  { lintErrorSrcLoc :: !(Maybe SrcLoc),
    lintErrorMessage :: !LintErrorMessage
  }
  deriving (Show, Eq)

data LintErrorMessage
  = LintErrorUndocumented
  | LintErrorEmptySetting
  | LintErrorNoReaderForArgument
  | LintErrorNoMetavarForArgument
  | LintErrorNoReaderForOption
  | LintErrorNoDashedForOption
  | LintErrorNoMetavarForOption
  | LintErrorNoDashedForSwitch
  | LintErrorNoReaderForEnvVar
  | LintErrorNoMetavarForEnvVar
  | LintErrorNoCommands
  deriving (Show, Eq)

renderLintErrors :: NonEmpty LintError -> [Chunk]
renderLintErrors =
  unlinesChunks
    . ([fore red "Setting parser is invalid:"] :)
    . map ("  " :)
    . concatMap (([] :) . renderLintError)

renderLintError :: LintError -> [[Chunk]]
renderLintError LintError {..} =
  concat
    [ [[fore red "Invalid Setting:"]],
      case lintErrorMessage of
        LintErrorUndocumented ->
          [["missing ", functionChunk "help", "."]]
        LintErrorEmptySetting ->
          concat
            [ [ [ "This ",
                  functionChunk "setting",
                  " parses nothing."
                ]
              ],
              [ [ "Add an ",
                  functionChunk "argument",
                  ", ",
                  functionChunk "switch",
                  ", ",
                  functionChunk "option",
                  ", ",
                  functionChunk "env",
                  ", ",
                  functionChunk "conf",
                  ", or ",
                  functionChunk "value",
                  "."
                ]
              ]
            ]
        LintErrorNoReaderForArgument ->
          [ [ functionChunk "argument",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoMetavarForArgument ->
          [ [ functionChunk "argument",
              " has no ",
              functionChunk "metavar",
              "."
            ]
          ]
        LintErrorNoReaderForOption ->
          [ [ functionChunk "option",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoDashedForOption ->
          [ [ functionChunk "option",
              " has no ",
              functionChunk "long",
              " or ",
              functionChunk "short",
              "."
            ]
          ]
        LintErrorNoMetavarForOption ->
          [ [ functionChunk "option",
              " has no ",
              functionChunk "metavar",
              "."
            ]
          ]
        LintErrorNoDashedForSwitch ->
          [ [ functionChunk "switch",
              " has no ",
              functionChunk "long",
              " or ",
              functionChunk "short",
              "."
            ]
          ]
        LintErrorNoReaderForEnvVar ->
          [ [ functionChunk "env",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoMetavarForEnvVar ->
          [ [ functionChunk "env",
              " has no ",
              functionChunk "metavar",
              "."
            ]
          ]
        LintErrorNoCommands ->
          [ [ functionChunk "commands",
              " was called with an empty list."
            ]
          ],
      maybe [] (pure . ("Defined at: " :) . pure . fore cyan . chunk . T.pack . prettySrcLoc) lintErrorSrcLoc
    ]

functionChunk :: Text -> Chunk
functionChunk = fore yellow . chunk

-- Put this in your test suite
lintParserTest :: Parser a -> IO ()
lintParserTest p = do
  case lintParser p of
    Nothing -> pure ()
    Just errs -> do
      tc <- getTerminalCapabilitiesFromHandle stderr
      hPutChunksLocaleWith tc stderr $ renderLintErrors errs
      exitFailure

lintParser :: Parser a -> Maybe (NonEmpty LintError)
lintParser = either Just (const Nothing) . validationToEither . go
  where
    go :: Parser a -> Validation LintError ()
    go = \case
      ParserPure _ -> pure ()
      ParserAp p1 p2 -> go p1 *> go p2
      ParserSelect p1 p2 -> go p1 *> go p2
      ParserEmpty -> pure ()
      ParserAlt p1 p2 -> go p1 *> go p2
      -- TODO lint if we try to read config or env under many/some?
      ParserMany p -> go p
      ParserCheck _ p -> go p
      ParserCommands ls -> do
        if null ls
          then validationFailure $ LintError Nothing LintErrorNoCommands
          else traverse_ (go . commandParser) ls
      ParserWithConfig p1 p2 -> go p1 *> go p2
      ParserSetting mLoc Setting {..} -> mapValidationFailure (LintError mLoc) $ do
        case settingHelp of
          Nothing ->
            -- Hidden values may be undocumented
            when (not settingHidden) $ validationFailure LintErrorUndocumented
          Just _ -> pure ()
        when
          ( and
              [ not settingTryArgument,
                isNothing settingSwitchValue,
                not settingTryOption,
                isNothing settingEnvVars,
                isNothing settingConfigVals,
                isNothing settingDefaultValue
              ]
          )
          $ validationFailure LintErrorEmptySetting
        when (settingTryArgument && null settingReaders) $
          validationFailure LintErrorNoReaderForArgument
        when (settingTryArgument && isNothing settingMetavar) $
          validationFailure LintErrorNoMetavarForArgument
        when (settingTryOption && null settingReaders) $
          validationFailure LintErrorNoReaderForOption
        when (settingTryOption && null settingDasheds) $
          validationFailure LintErrorNoDashedForOption
        when (settingTryOption && isNothing settingMetavar) $
          validationFailure LintErrorNoMetavarForOption
        when (isJust settingSwitchValue && null settingDasheds) $
          validationFailure LintErrorNoDashedForSwitch
        when (isJust settingEnvVars && null settingReaders) $
          validationFailure LintErrorNoReaderForEnvVar
        when (isJust settingEnvVars && isNothing settingMetavar) $
          validationFailure LintErrorNoMetavarForEnvVar
