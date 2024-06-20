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
  | ParseErrorCheckFailed !String
  | ParseErrorMissingArgument !(Maybe OptDoc)
  | ParseErrorArgumentRead !(NonEmpty String)
  | ParseErrorMissingOption !(Maybe OptDoc)
  | ParseErrorOptionRead !(NonEmpty String)
  | ParseErrorMissingEnvVar !(Maybe EnvDoc)
  | ParseErrorEnvRead !(NonEmpty String)
  | ParseErrorMissingSwitch !(Maybe OptDoc)
  | ParseErrorMissingConfVal !(Maybe ConfDoc)
  | ParseErrorConfigRead !String
  | ParseErrorMissingCommand ![String]
  | ParseErrorUnrecognisedCommand !String ![String]
  deriving (Show, Eq)

-- Whether the other side of an 'Alt' should be tried if we find this error.
errorIsForgivable :: ParseError -> Bool
errorIsForgivable = \case
  ParseErrorEmpty -> True
  ParseErrorEmptySetting -> False
  ParseErrorCheckFailed _ -> False
  ParseErrorMissingArgument _ -> True
  ParseErrorArgumentRead _ -> False
  ParseErrorMissingSwitch _ -> True
  ParseErrorOptionRead _ -> False
  ParseErrorMissingOption _ -> True
  ParseErrorMissingEnvVar _ -> True
  ParseErrorEnvRead _ -> False
  ParseErrorMissingConfVal _ -> True
  ParseErrorConfigRead _ -> False
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
  ParseErrorCheckFailed err ->
    [["Check failed:"], [chunk $ T.pack err]]
  ParseErrorMissingArgument o ->
    ["Missing argument:" : unwordsChunks (maybe (error "TODO") renderOptDocLong o)]
  ParseErrorArgumentRead errs ->
    ["Failed to read argument:"] : map (\err -> [chunk $ T.pack err]) (NE.toList errs)
  ParseErrorMissingOption o ->
    ["Missing option:" : unwordsChunks (maybe (error "TODO") renderOptDocLong o)]
  ParseErrorMissingSwitch o ->
    ["Missing switch:" : unwordsChunks (maybe (error "TODO") renderOptDocLong o)]
  ParseErrorOptionRead errs ->
    ["Failed to read option:"] : map (\err -> [chunk $ T.pack err]) (NE.toList errs)
  ParseErrorMissingEnvVar md ->
    [["Missing option: ", maybe (error "TODO") (chunk . T.pack . show) md]]
  ParseErrorEnvRead errs ->
    ["Failed to read env var:"] : map (\err -> [chunk $ T.pack err]) (NE.toList errs)
  ParseErrorMissingConfVal md ->
    ["Missing config value: "] : maybe (error "TODO") renderConfDoc md
  ParseErrorConfigRead s ->
    [["Failed to parse configuration:", chunk $ T.pack $ show s]]
  ParseErrorMissingCommand cs ->
    [ ["Missing command, available commands:"],
      unwordsChunks $ map (pure . fore yellow . chunk . T.pack) cs
    ]
  ParseErrorUnrecognisedCommand c cs ->
    [ [fore red "Unrecognised command: ", fore yellow $ chunk (T.pack c)],
      [fore blue "available commands:"],
      unwordsChunks $ map (pure . fore yellow . chunk . T.pack) cs
    ]
