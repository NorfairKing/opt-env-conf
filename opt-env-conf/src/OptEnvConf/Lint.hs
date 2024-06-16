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
import OptEnvConf.Doc
import OptEnvConf.Parser
import OptEnvConf.Setting
import OptEnvConf.Validation
import System.Exit
import System.IO (stderr)
import Text.Colour
import Text.Colour.Capabilities.FromEnv

data LintError = LintError
  { lintErrorSetDoc :: !(Maybe SetDoc),
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
    [ case lintErrorMessage of
        LintErrorUndocumented ->
          [[errorChunk, " ", "Undocumented setting"]]
        LintErrorEmptySetting ->
          concat
            [ [ [ errorChunk,
                  " This ",
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
          [ [ errorChunk,
              " ",
              functionChunk "argument",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoMetavarForArgument ->
          [ [ errorChunk,
              " ",
              functionChunk "argument",
              " has no ",
              functionChunk "metavar",
              "."
            ]
          ]
        LintErrorNoReaderForOption ->
          [ [ errorChunk,
              " ",
              functionChunk "option",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoDashedForOption ->
          [ [ errorChunk,
              " ",
              functionChunk "option",
              " has no ",
              functionChunk "long",
              " or ",
              functionChunk "short",
              "."
            ]
          ]
        LintErrorNoMetavarForOption ->
          [ [ errorChunk,
              " ",
              functionChunk "option",
              " has no ",
              functionChunk "metavar",
              "."
            ]
          ]
        LintErrorNoDashedForSwitch ->
          [ [ errorChunk,
              " ",
              functionChunk "switch",
              " has no ",
              functionChunk "long",
              " or ",
              functionChunk "short",
              "."
            ]
          ]
        LintErrorNoReaderForEnvVar ->
          [ [ errorChunk,
              " ",
              functionChunk "env",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoMetavarForEnvVar ->
          [ [ errorChunk,
              " ",
              functionChunk "env",
              " has no ",
              functionChunk "metavar",
              "."
            ]
          ]
        LintErrorNoCommands ->
          [ [ errorChunk,
              " ",
              functionChunk "commands",
              " was called with an empty list."
            ]
          ],
      maybe [] renderSetDoc lintErrorSetDoc
    ]

errorChunk :: Chunk
errorChunk = fore red "Error:"

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
      ParserSetting s@Setting {..} -> mapValidationFailure (LintError (settingSetDoc s)) $ do
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
