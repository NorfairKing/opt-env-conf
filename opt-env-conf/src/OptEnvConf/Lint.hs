{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Lint
  ( LintError (..),
    LintErrorMessage (..),
    renderLintErrors,
    renderLintError,
    lintParser,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (SrcLoc, prettySrcLoc)
import OptEnvConf.Args
import OptEnvConf.Parser
import qualified OptEnvConf.Reader as OptEnvConf
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
  | LintErrorDashInShort
  | LintErrorDashInLong !(NonEmpty Char)
  | LintErrorNoReaderForArgument
  | LintErrorNoMetavarForArgument
  | LintErrorNoReaderForOption
  | LintErrorNoDashedForOption
  | LintErrorNoMetavarForOption
  | LintErrorNoDashedForSwitch
  | LintErrorNoOptionOrSwitchForDashed
  | LintErrorNoReaderForEnvVar
  | LintErrorNoMetavarForEnvVar
  | LintErrorNoCommands
  | LintErrorUnreadableExample !String
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
        LintErrorDashInShort ->
          [ [functionChunk "short", " may not contain a '-'."],
            ["Found ", functionChunk "short", " '-'."]
          ]
        LintErrorDashInLong s ->
          [ [functionChunk "long", " may not start with a '-'."],
            ["Found ", functionChunk "long", " ", chunk $ T.pack $ show $ NE.toList s, "."],
            [ "Try ",
              functionChunk "long",
              " ",
              chunk $
                T.pack $
                  show $
                    let go = \case
                          [] -> []
                          '-' : cs -> go cs
                          c : cs -> c : cs
                     in go $ NE.toList s,
              " instead."
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
        LintErrorNoOptionOrSwitchForDashed ->
          [ [ functionChunk "long",
              " or ",
              functionChunk "short",
              " has no ",
              functionChunk "option",
              " or ",
              functionChunk "switch",
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
        LintErrorUnreadableExample e ->
          [ [functionChunk "example", " was called with an example that none of the ", functionChunk "reader", "s succeed in reading."],
            ["Example: ", chunk $ T.pack e]
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
      ParserEmpty _ -> pure ()
      ParserAlt p1 p2 -> go p1 *> go p2
      -- TODO lint if we don't try to parse anything consuming under many.
      ParserMany p -> go p
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ p -> go p
      ParserCommands mLoc ls -> do
        if null ls
          then validationTFailure $ LintError mLoc LintErrorNoCommands
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
                isNothing settingConfigVals
              ]
          )
          $ validationTFailure LintErrorEmptySetting
        for_ settingDasheds $ \case
          DashedLong cs@('-' :| _) -> validationTFailure $ LintErrorDashInLong cs
          DashedShort '-' -> validationTFailure LintErrorDashInShort
          _ -> pure ()
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
        when (not settingTryOption && isNothing settingSwitchValue && not (null settingDasheds)) $
          validationTFailure LintErrorNoOptionOrSwitchForDashed
        when (isJust settingEnvVars && null settingReaders) $
          validationTFailure LintErrorNoReaderForEnvVar
        when (isJust settingEnvVars && not settingHidden && isNothing settingMetavar) $
          validationTFailure LintErrorNoMetavarForEnvVar
        for_ settingExamples $ \e ->
          when (not $ any (\r -> isRight $ OptEnvConf.runReader r e) settingReaders) $
            validationTFailure $
              LintErrorUnreadableExample e
        hasConfig <- ask
        when (isJust settingConfigVals && not hasConfig) $
          validationTFailure LintErrorConfigWithoutLoad
