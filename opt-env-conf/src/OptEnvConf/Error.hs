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
  | ParseErrorMissingArgument !OptDoc
  | ParseErrorArgumentRead !String
  | ParseErrorMissingOption !OptDoc
  | ParseErrorOptionRead !String
  | ParseErrorMissingEnvVar !EnvDoc
  | ParseErrorEnvRead !String
  | ParseErrorMissingSwitch !OptDoc
  | ParseErrorMissingConfig !String
  | ParseErrorConfigRead !String
  | ParseErrorRequired
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
  ParseErrorMissingArgument o ->
    ["Missing argument:"] : renderOptDocLong o
  ParseErrorArgumentRead s ->
    [["Failed to read argument: ", chunk $ T.pack $ show s]]
  ParseErrorMissingOption o ->
    ["Missing option:"] : renderOptDocLong o
  ParseErrorMissingSwitch o ->
    ["Missing switch:"] : renderOptDocLong o
  ParseErrorOptionRead s ->
    [["Failed to read option: ", chunk $ T.pack $ show s]]
  ParseErrorMissingEnvVar v ->
    [["Missing option: ", chunk $ T.pack $ show v]]
  ParseErrorEnvRead s ->
    [["Failed to env var: ", chunk $ T.pack $ show s]]
  ParseErrorMissingConfig v ->
    [["Missing configuration: ", chunk $ T.pack $ show v]]
  ParseErrorConfigRead s ->
    [["Failed to parse configuration:", chunk $ T.pack $ show s]]
  ParseErrorRequired ->
    [["Required"]]
