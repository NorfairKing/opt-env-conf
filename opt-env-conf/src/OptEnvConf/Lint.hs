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

data LintError
  = LintErrorUndocumented
  | LintErrorEmptySetting !(Maybe Help)
  | LintErrorNoReaderForArgument !(Maybe Help)
  | LintErrorNoReaderForOption !(Maybe Help)
  | LintErrorNoDashedForOption !(Maybe Help)
  | LintErrorNoMetavarForOption !(Maybe Help)
  | LintErrorNoDashedForSwitch !(Maybe Help)
  | LintErrorNoReaderForEnvVar !(Maybe Help)
  | LintErrorNoMetavarForEnvVar !(Maybe Help)
  deriving (Show, Eq)

renderLintErrors :: NonEmpty LintError -> [Chunk]
renderLintErrors =
  unlinesChunks
    . ([fore red "Setting parser is invalid:"] :)
    . map ("  " :)
    . concatMap (([] :) . renderLintError)

renderLintError :: LintError -> [[Chunk]]
renderLintError = \case
  LintErrorUndocumented ->
    [ [errorChunk, " ", "Undocumented setting."]
    ]
  LintErrorNoReaderForArgument h ->
    [ errorChunk,
      " ",
      functionChunk "argument",
      " has no ",
      functionChunk "reader",
      ":"
    ]
      : mHelpLines h
  LintErrorEmptySetting h ->
    concat
      [ [ [ errorChunk,
            " This ",
            functionChunk "setting",
            " parses nothing:"
          ]
        ],
        mHelpLines h,
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
            ":"
          ]
        ]
      ]
  LintErrorNoReaderForOption h ->
    [ errorChunk,
      " ",
      functionChunk "option",
      " has no ",
      functionChunk "reader",
      ":"
    ]
      : mHelpLines h
  LintErrorNoDashedForOption h ->
    [ errorChunk,
      " ",
      functionChunk "option",
      " has no ",
      functionChunk "long",
      " or ",
      functionChunk "short",
      ":"
    ]
      : mHelpLines h
  LintErrorNoMetavarForOption h ->
    [ errorChunk,
      " ",
      functionChunk "option",
      " has no ",
      functionChunk "metavar",
      ":"
    ]
      : mHelpLines h
  LintErrorNoDashedForSwitch h ->
    [ errorChunk,
      " ",
      functionChunk "switch",
      " has no ",
      functionChunk "long",
      " or ",
      functionChunk "short",
      ":"
    ]
      : mHelpLines h
  LintErrorNoReaderForEnvVar h ->
    [ errorChunk,
      " ",
      functionChunk "env",
      " has no ",
      functionChunk "reader",
      ":"
    ]
      : mHelpLines h
  LintErrorNoMetavarForEnvVar h ->
    [ errorChunk,
      " ",
      functionChunk "env",
      " has no ",
      functionChunk "metavar",
      ":"
    ]
      : mHelpLines h

errorChunk :: Chunk
errorChunk = fore red "Error:"

functionChunk :: Text -> Chunk
functionChunk = fore yellow . chunk

mHelpLines :: Maybe Help -> [[Chunk]]
mHelpLines =
  maybe
    [["This setting is undocument, so we cannot refer to it."]]
    helpLines

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
      ParserFmap _ p -> go p
      ParserAp p1 p2 -> go p1 *> go p2
      ParserSelect p1 p2 -> go p1 *> go p2
      ParserEmpty -> pure ()
      ParserAlt p1 p2 -> go p1 *> go p2
      -- TODO lint if we try to read config or env under many/some?
      ParserMany p -> go p
      ParserMapIO _ p -> go p
      ParserWithConfig p1 p2 -> go p1 *> go p2
      ParserSetting Setting {..} -> do
        case settingHelp of
          Nothing ->
            -- Hidden values may be undocumented
            when (not settingHidden) $
              validationFailure LintErrorUndocumented
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
          $ validationFailure
          $ LintErrorEmptySetting settingHelp
        when (settingTryArgument && null settingReaders) $
          validationFailure $
            LintErrorNoReaderForArgument settingHelp
        when (settingTryOption && null settingReaders) $
          validationFailure $
            LintErrorNoReaderForOption settingHelp
        when (settingTryOption && null settingDasheds) $
          validationFailure $
            LintErrorNoDashedForOption settingHelp
        when (settingTryOption && isNothing settingMetavar) $
          validationFailure $
            LintErrorNoMetavarForOption settingHelp
        when (isJust settingSwitchValue && null settingDasheds) $
          validationFailure $
            LintErrorNoDashedForSwitch settingHelp
        when (isJust settingEnvVars && null settingReaders) $
          validationFailure $
            LintErrorNoReaderForEnvVar settingHelp
        when (isJust settingEnvVars && isNothing settingMetavar) $
          validationFailure $
            LintErrorNoMetavarForEnvVar settingHelp
