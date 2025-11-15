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
import qualified Data.Text as T
import GHC.Stack (SrcLoc, prettySrcLoc)
import OptEnvConf.Args
import OptEnvConf.Output
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
  | LintErrorUnknownDefaultCommand !String
  | LintErrorUnreadableExample !String
  | LintErrorConfigWithoutLoad
  | LintErrorManyInfinite

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
              " or ",
              functionChunk "name",
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
              " or ",
              functionChunk "name",
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
              " or ",
              functionChunk "name",
              " has no ",
              functionChunk "reader",
              "."
            ]
          ]
        LintErrorNoMetavarForEnvVar ->
          [ [ functionChunk "env",
              " or ",
              functionChunk "name",
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
        LintErrorUnknownDefaultCommand c ->
          [ [ functionChunk "defaultCommand",
              " was called with an unknown command: ",
              commandChunk c
            ]
          ]
        LintErrorUnreadableExample e ->
          [ [functionChunk "example", " was called with an example that none of the ", functionChunk "reader", "s succeed in reading."],
            ["Example: ", chunk $ T.pack e]
          ]
        LintErrorConfigWithoutLoad ->
          [ [ functionChunk "conf",
              " or ",
              functionChunk "name",
              " was called with no way to load configuration."
            ],
            [ "You can load configuration with ",
              functionChunk "withConfig",
              ", or explicitly not load any configuration with ",
              functionChunk "withoutConfig",
              "."
            ]
          ]
        LintErrorManyInfinite ->
          [ [ functionChunk "many",
              " or ",
              functionChunk "some",
              " was called with a parser that may succeed without consuming anything."
            ],
            ["This is not allowed because the parser would run infinitely."]
          ],
      maybe [] (pure . ("Defined at: " :) . pure . fore cyan . chunk . T.pack . prettySrcLoc) lintErrorSrcLoc
    ]

lintParser :: Parser a -> Maybe (NonEmpty LintError)
lintParser =
  either Just (const Nothing)
    . validationToEither
    . (`runReader` False) -- Set to true for parsers that have a way to load conf
    . runValidationT
    . go
  where
    -- Returns whether 'many' is allowed.
    -- 'many' is allowed only when every parse below consumes something.
    go :: Parser a -> ValidationT LintError (Reader Bool) Bool
    go = \case
      ParserPure _ -> pure False
      ParserAp p1 p2 -> do
        c1 <- go p1
        c2 <- go p2
        pure (c1 || c2)
      ParserSelect p1 p2 -> do
        c1 <- go p1
        c2 <- go p2
        pure (c1 || c2) -- TODO: is this right?
      ParserEmpty _ -> pure True
      ParserAlt p1 p2 -> do
        c1 <- go p1
        c2 <- go p2
        pure (c1 && c2) -- TODO: is this right?
        -- TODO lint if we don't try to parse anything consuming under many.
      ParserMany mLoc p -> do
        c <- go p
        when (not c) $
          mapValidationTFailure (LintError mLoc) $
            validationTFailure LintErrorManyInfinite
        pure c
      ParserSome mLoc p -> do
        c <- go p
        when (not c) $
          mapValidationTFailure (LintError mLoc) $
            validationTFailure LintErrorManyInfinite
        pure c
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ _ p -> go p
      ParserRequireCapability _ _ p -> go p
      ParserCommands mLoc mDefault cs -> do
        if null cs
          then validationTFailure $ LintError mLoc LintErrorNoCommands
          else do
            for_ mDefault $ \d ->
              when (isNothing (find ((== d) . commandArg) cs)) $
                validationTFailure $
                  LintError mLoc $
                    LintErrorUnknownDefaultCommand d
            and <$> traverse (go . commandParser) cs -- TODO is this right?
      ParserWithConfig _ p1 p2 -> do
        c1 <- go p1
        c2 <- local (const True) (go p2)
        pure $ c1 || c2
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
          let canRead r = isRight $ OptEnvConf.runReader r e
           in when ((settingTryArgument || settingTryOption) && not (any canRead settingReaders)) $
                validationTFailure $
                  LintErrorUnreadableExample e
        hasConfig <- ask
        when (isJust settingConfigVals && not hasConfig) $
          validationTFailure LintErrorConfigWithoutLoad
        pure $
          -- 'many' is only allowed if something is being consumed and it's
          -- impossible for nothing to be consumed.
          and
            [ settingTryArgument || settingTryOption || isJust settingSwitchValue,
              null settingEnvVars,
              null settingConfigVals
            ]
