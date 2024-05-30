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
  | ParseErrorArgumentRead !String
  | ParseErrorOptionRead !String
  | ParseErrorRequired
  | ParseErrorMissingArgument !OptDoc
  | ParseErrorMissingOption !OptDoc
  | ParseErrorConfigParseError !String
  deriving (Show, Eq)

renderErrors :: NonEmpty ParseError -> [Chunk]
renderErrors = unlinesChunks . concatMap renderError . NE.toList

renderError :: ParseError -> [[Chunk]]
renderError = \case
  ParseErrorEmpty ->
    [["Hit the 'empty' case of the Parser type, this should not happen."]]
  ParseErrorArgumentRead s ->
    [["Failed to read argument:", chunk $ T.pack $ show s]]
  ParseErrorOptionRead s ->
    [["Failed to read option:", chunk $ T.pack $ show s]]
  ParseErrorRequired ->
    [["Required"]]
  ParseErrorMissingArgument o ->
    ["Missing argument:"] : renderOptDocLong o
  ParseErrorMissingOption o ->
    ["Missing option:"] : renderOptDocLong o
  ParseErrorConfigParseError s ->
    [["Failed to parse configuration:", chunk $ T.pack $ show s]]
