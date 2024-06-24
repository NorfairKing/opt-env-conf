{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Lint
  ( LintError (..),
    renderLintErrors,
    renderLintError,
    lintParser,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (SrcLoc, prettySrcLoc)
import OptEnvConf.Parser
import OptEnvConf.Setting
import OptEnvConf.Validation
import Text.Colour

data LintError = LintError
  { lintErrorSrcLoc :: !(Maybe SrcLoc),
    lintErrorMessage :: !LintErrorMessage
  }

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
  | LintErrorConfigWithoutLoad

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
          ]
        LintErrorConfigWithoutLoad ->
          [ [ functionChunk "conf",
              " was called with no way to load configuration."
            ]
          ],
      maybe [] (pure . ("Defined at: " :) . pure . fore cyan . chunk . T.pack . prettySrcLoc) lintErrorSrcLoc
    ]

functionChunk :: Text -> Chunk
functionChunk = fore yellow . chunk

lintParser :: Parser a -> Maybe (NonEmpty LintError)
lintParser =
  either Just (const Nothing)
    . validationToEither
    . (`runReader` False) -- Set to true for parsers that have a way to load conf
    . runValidationT
    . go
  where
    go :: Parser a -> ValidationT LintError (Reader Bool) ()
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
          then validationTFailure $ LintError Nothing LintErrorNoCommands
          else traverse_ (go . commandParser) ls
      ParserWithConfig p1 p2 -> go p1 *> local (const True) (go p2)
      ParserSetting mLoc Setting {..} -> mapValidationTFailure (LintError mLoc) $ do
        case settingHelp of
          Nothing ->
            -- Hidden values may be undocumented
            when (not settingHidden) $ validationTFailure LintErrorUndocumented
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
          $ validationTFailure LintErrorEmptySetting
        when (settingTryArgument && null settingReaders) $
          validationTFailure LintErrorNoReaderForArgument
        when (settingTryArgument && not settingHidden && isNothing settingMetavar) $
          validationTFailure LintErrorNoMetavarForArgument
        when (settingTryOption && null settingReaders) $
          validationTFailure LintErrorNoReaderForOption
        when (settingTryOption && null settingDasheds) $
          validationTFailure LintErrorNoDashedForOption
        when (settingTryOption && not settingHidden && isNothing settingMetavar) $
          validationTFailure LintErrorNoMetavarForOption
        when (isJust settingSwitchValue && null settingDasheds) $
          validationTFailure LintErrorNoDashedForSwitch
        when (isJust settingEnvVars && null settingReaders) $
          validationTFailure LintErrorNoReaderForEnvVar
        when (isJust settingEnvVars && not settingHidden && isNothing settingMetavar) $
          validationTFailure LintErrorNoMetavarForEnvVar
        hasConfig <- ask
        when (isJust settingConfigVals && not hasConfig) $
          validationTFailure LintErrorConfigWithoutLoad
