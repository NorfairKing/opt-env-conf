{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Error where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import OptEnvConf.Doc
import Text.Colour

data ParseError
  = ParseErrorEmpty
  | ParseErrorEmptySetting
  | ParseErrorCheckFailed !Bool !String
  | ParseErrorMissingArgument !(Maybe OptDoc)
  | ParseErrorArgumentRead !(Maybe OptDoc) !(NonEmpty String)
  | ParseErrorMissingOption !(Maybe OptDoc)
  | ParseErrorOptionRead !(Maybe OptDoc) !(NonEmpty String)
  | ParseErrorMissingEnvVar !(Maybe EnvDoc)
  | ParseErrorEnvRead !(Maybe EnvDoc) !(NonEmpty String)
  | ParseErrorMissingSwitch !(Maybe OptDoc)
  | ParseErrorMissingConfVal !(Maybe ConfDoc)
  | ParseErrorConfigRead !(Maybe ConfDoc) !String
  | ParseErrorMissingCommand ![String]
  | ParseErrorUnrecognisedCommand !String ![String]
  deriving (Show)

-- Whether the other side of an 'Alt' should be tried if we find this error.
errorIsForgivable :: ParseError -> Bool
errorIsForgivable = \case
  ParseErrorEmpty -> True
  ParseErrorEmptySetting -> False
  ParseErrorCheckFailed forgivable _ -> forgivable
  ParseErrorMissingArgument _ -> True
  ParseErrorArgumentRead _ _ -> False
  ParseErrorMissingSwitch _ -> True
  ParseErrorOptionRead _ _ -> False
  ParseErrorMissingOption _ -> True
  ParseErrorMissingEnvVar _ -> True
  ParseErrorEnvRead _ _ -> False
  ParseErrorMissingConfVal _ -> True
  ParseErrorConfigRead _ _ -> False
  ParseErrorMissingCommand cs -> not $ null cs
  ParseErrorUnrecognisedCommand _ _ -> False

renderErrors :: NonEmpty ParseError -> [Chunk]
renderErrors = unlinesChunks . concatMap renderError . NE.toList

renderError :: ParseError -> [[Chunk]]
renderError = \case
  ParseErrorEmpty ->
    [["Hit the 'empty' case of the Parser type, this should not happen."]]
  ParseErrorEmptySetting ->
    [["This setting has not been configured to be able to parse anything."]]
  ParseErrorCheckFailed _ err ->
    [["Check failed: "], [chunk $ T.pack err]]
  ParseErrorMissingArgument o ->
    [ "Missing argument: "
        : unwordsChunks (maybe [] renderOptDocLong o)
    ]
  ParseErrorArgumentRead md errs ->
    ["Failed to read argument: "]
      : unwordsChunks (maybe [] renderOptDocLong md)
      : map (\err -> [chunk $ T.pack err]) (NE.toList errs)
  ParseErrorMissingOption o ->
    ["Missing option: " : unwordsChunks (maybe [] renderOptDocLong o)]
  ParseErrorMissingSwitch o ->
    ["Missing switch: " : unwordsChunks (maybe [] renderOptDocLong o)]
  ParseErrorOptionRead md errs ->
    ["Failed to read option: "]
      : unwordsChunks (maybe [] renderOptDocLong md)
      : map (\err -> [chunk $ T.pack err]) (NE.toList errs)
  ParseErrorMissingEnvVar md ->
    ["Missing option: "]
      : maybe [] renderEnvDoc md
  ParseErrorEnvRead md errs ->
    ["Failed to read env var: "]
      : maybe [] renderEnvDoc md
      ++ map (\err -> [chunk $ T.pack err]) (NE.toList errs)
  ParseErrorMissingConfVal md ->
    ["Missing config value: "] : maybe [] renderConfDoc md
  ParseErrorConfigRead md s ->
    ["Failed to parse configuration: "]
      : maybe [] renderConfDoc md
      ++ [[chunk $ T.pack $ show s]]
  ParseErrorMissingCommand cs ->
    [ ["Missing command, available commands:"],
      unwordsChunks $ map (pure . fore yellow . chunk . T.pack) cs
    ]
  ParseErrorUnrecognisedCommand c cs ->
    [ [fore red "Unrecognised command: ", fore yellow $ chunk (T.pack c)],
      [fore blue "available commands:"],
      unwordsChunks $ map (pure . fore yellow . chunk . T.pack) cs
    ]
