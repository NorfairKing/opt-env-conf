{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Error where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import OptEnvConf.ArgMap (Opt (..), renderDashed)
import OptEnvConf.Doc
import Text.Colour

data ParseError
  = ParseErrorUnrecognised !Opt
  | ParseErrorEmpty
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
  ParseErrorUnrecognised opt ->
    case opt of
      OptArg s ->
        [ ["Unexpected argument: ", chunk $ T.pack $ show s],
          ["No arguments are expected to be parsed."]
        ]
      OptSwitch d ->
        [ ["Unexpected switch: ", chunk $ T.pack $ renderDashed d],
          ["This option is not expected to be parsed."]
        ]
      OptOption d v ->
        [ ["Unexpected option: ", chunk $ T.pack $ unwords [renderDashed d, show v]],
          ["This option is not expected to be parsed."]
        ]
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
