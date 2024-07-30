{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Doc
  ( renderVersionPage,
    parserDocs,
    commandParserDocs,
    renderHelpPage,
    renderCommandHelpPage,
    renderManPage,
    renderReferenceDocumentation,
    parserOptDocs,
    renderLongOptDocs,
    renderShortOptDocs,
    parserEnvDocs,
    renderEnvDocs,
    parserConfDocs,
    renderConfDocs,

    -- * Internal
    AnyDocs (..),
    SetDoc (..),
    OptDoc (..),
    EnvDoc (..),
    ConfDoc (..),
    CommandDoc (..),
    settingSetDoc,
    renderSetDoc,
    settingOptDoc,
    renderOptDocLong,
    settingEnvDoc,
    renderEnvDoc,
    settingConfDoc,
    renderConfDoc,
    helpLines,
  )
where

import Autodocodec.Schema
import Autodocodec.Yaml.Schema
import Control.Monad
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import OptEnvConf.Args (Dashed (..))
import OptEnvConf.Output
import OptEnvConf.Parser
import OptEnvConf.Setting
import Text.Colour
import Text.Colour.Layout

data SetDoc = SetDoc
  { setDocTryArgument :: !Bool,
    setDocTrySwitch :: !Bool,
    setDocTryOption :: !Bool,
    setDocDasheds :: ![Dashed],
    setDocEnvVars :: !(Maybe (NonEmpty String)),
    setDocConfKeys :: !(Maybe (NonEmpty (NonEmpty String, JSONSchema))),
    setDocDefault :: !(Maybe String),
    setDocExamples :: ![String],
    setDocMetavar :: !(Maybe Metavar),
    setDocHelp :: !(Maybe String)
  }
  deriving (Show)

data OptDoc = OptDoc
  { optDocTryArgument :: !Bool,
    optDocTrySwitch :: !Bool,
    optDocTryOption :: !Bool,
    optDocDasheds :: ![Dashed],
    optDocDefault :: !(Maybe String),
    optDocExamples :: ![String],
    optDocMetavar :: !(Maybe Metavar),
    optDocHelp :: !(Maybe String)
  }
  deriving (Show)

data EnvDoc = EnvDoc
  { envDocVars :: !(NonEmpty String),
    envDocDefault :: !(Maybe String),
    envDocExamples :: ![String],
    envDocMetavar :: !(Maybe Metavar),
    envDocHelp :: !(Maybe String)
  }
  deriving (Show)

data ConfDoc = ConfDoc
  { confDocKeys :: !(NonEmpty (NonEmpty String, JSONSchema)),
    confDocDefault :: !(Maybe String),
    confDocExamples :: ![String],
    confDocHelp :: !(Maybe String)
  }
  deriving (Show)

data AnyDocs a
  = AnyDocsCommands [CommandDoc a]
  | AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle !a
  deriving (Show)

data CommandDoc a = CommandDoc
  { commandDocArgument :: String,
    commandDocHelp :: Help,
    commandDocs :: AnyDocs a
  }
  deriving (Show)

mapMaybeDocs :: (a -> Maybe b) -> AnyDocs a -> AnyDocs b
mapMaybeDocs func = simplifyAnyDocs . go
  where
    go = \case
      AnyDocsCommands cs -> AnyDocsCommands $ map goCommandDoc cs
      AnyDocsAnd ds -> AnyDocsAnd $ map go ds
      AnyDocsOr ds -> AnyDocsOr $ map go ds
      AnyDocsSingle d -> maybe (AnyDocsAnd []) AnyDocsSingle $ func d

    goCommandDoc cd = cd {commandDocs = go (commandDocs cd)}

simplifyAnyDocs :: AnyDocs a -> AnyDocs a
simplifyAnyDocs = go
  where
    go = \case
      AnyDocsCommands cs -> AnyDocsCommands $ map goDoc cs
      AnyDocsAnd ds -> case concatMap goAnd ds of
        [a] -> a
        as -> AnyDocsAnd as
      AnyDocsOr ds -> case concatMap goOr ds of
        [a] -> a
        as -> AnyDocsOr as
      AnyDocsSingle v -> AnyDocsSingle v

    goDoc cd = cd {commandDocs = go (commandDocs cd)}

    goAnd = \case
      AnyDocsCommands c -> [AnyDocsCommands $ map goDoc c]
      AnyDocsAnd ds -> concatMap (goAnd . go) ds
      AnyDocsOr [] -> []
      ds -> [go ds]

    goOr = \case
      AnyDocsCommands c -> [AnyDocsCommands $ map goDoc c]
      AnyDocsOr ds -> concatMap (goOr . go) ds
      AnyDocsAnd [] -> []
      ds -> [go ds]

-- | Derive parser documentation
parserDocs :: Parser a -> AnyDocs SetDoc
parserDocs = simplifyAnyDocs . go
  where
    noDocs = AnyDocsAnd []
    go :: Parser a -> AnyDocs SetDoc
    go = \case
      ParserPure _ -> noDocs
      ParserAp pf pa -> AnyDocsAnd [go pf, go pa]
      ParserSelect p1 p2 -> AnyDocsAnd [go p1, go p2]
      ParserEmpty _ -> AnyDocsOr []
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserMany p -> go p -- TODO: is this right?
      ParserSome p -> go p -- TODO: is this right?
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ p -> go p
      ParserCommands _ cs -> AnyDocsCommands $ map commandParserDocs cs
      ParserWithConfig _ p1 p2 -> AnyDocsAnd [go p1, go p2] -- TODO: is this right? Maybe we want to document that it's not a pure parser?
      ParserSetting _ set -> maybe noDocs AnyDocsSingle $ settingSetDoc set

commandParserDocs :: Command a -> CommandDoc SetDoc
commandParserDocs Command {..} =
  CommandDoc
    { commandDocArgument = commandArg,
      commandDocHelp = commandHelp,
      commandDocs = parserDocs commandParser
    }

settingSetDoc :: Setting a -> Maybe SetDoc
settingSetDoc Setting {..} = do
  guard $ not settingHidden
  let setDocDasheds = settingDasheds
  let setDocTryArgument = settingTryArgument
  let setDocTrySwitch = isJust settingSwitchValue
  let setDocTryOption = settingTryOption
  let setDocEnvVars = settingEnvVars
  let setDocConfKeys =
        NE.map
          ( \ConfigValSetting {..} ->
              (configValSettingPath, jsonSchemaVia configValSettingCodec)
          )
          <$> settingConfigVals
  let setDocDefault = snd <$> settingDefaultValue
  let setDocExamples = settingExamples
  let setDocMetavar = settingMetavar
  let setDocHelp = settingHelp
  pure SetDoc {..}

settingOptDoc :: Setting a -> Maybe OptDoc
settingOptDoc = settingSetDoc >=> setDocOptDoc

renderSetDoc :: SetDoc -> [[Chunk]]
renderSetDoc setDoc =
  concat
    [ renderSetDocHeader (setDocHelp setDoc),
      renderSetDocWithoutHeader setDoc,
      [[]]
    ]

renderSetDocHeader :: Maybe Help -> [[Chunk]]
renderSetDocHeader = maybe [[fore red "undocumented"]] helpLines

renderSetDocWithoutHeader :: SetDoc -> [[Chunk]]
renderSetDocWithoutHeader SetDoc {..} =
  concat
    [ [ defaultValueChunks dv
        | dv <- maybeToList setDocDefault
      ],
      [ exampleValuesChunks setDocExamples
        | not (null setDocExamples)
      ],
      [ unwordsChunks
          [ ["argument:"],
            [mMetavarChunk setDocMetavar]
          ]
        | setDocTryArgument
      ],
      [ unwordsChunks
          [ ["switch:"],
            dashedChunksNE dasheds
          ]
        | setDocTrySwitch,
          dasheds <- maybeToList (NE.nonEmpty setDocDasheds)
      ],
      [ unwordsChunks
          [ ["option:"],
            dashedChunksNE dasheds
              ++ [" ", mMetavarChunk setDocMetavar]
          ]
        | setDocTryOption,
          dasheds <- maybeToList (NE.nonEmpty setDocDasheds)
      ],
      [ unwordsChunks
          [ ["env:"],
            envVarChunksNE vars
              ++ [" ", mMetavarChunk setDocMetavar]
          ]
        | vars <- maybeToList setDocEnvVars
      ],
      concat
        [ concatMap
            ( \(key, schema) ->
                case jsonSchemaChunkLines schema of
                  [line] ->
                    [["config: ", confValChunk key, ": "] ++ line]
                  ls ->
                    ["config:"]
                      : indent
                        ( case ls of
                            [] -> [["TODO"]]
                            (l : ll) ->
                              ([confValChunk key, ": "] ++ l)
                                : indent ll
                        )
            )
            (NE.toList confs)
          | confs <- maybeToList setDocConfKeys
        ]
    ]

helpLines :: Help -> [[Chunk]]
helpLines = map (map (fore blue)) . stringLines

progDescLines :: String -> [[Chunk]]
progDescLines = stringLines

-- | Render the output of `--render-man-page` for reading with @man@
renderManPage ::
  String ->
  Version ->
  String ->
  AnyDocs SetDoc ->
  [Chunk]
renderManPage progname version progDesc docs =
  let optDocs = docsToOptDocs docs
      envDocs = docsToEnvDocs docs
      confDocs = docsToConfDocs docs
      commandDocs = docsToCommandDocs docs
   in unlinesChunks $
        -- See https://man.openbsd.org/mdoc#MACRO_OVERVIEW
        concat
          [ [ -- Document date
              [".Dd $Mdocdate$"],
              -- Document title
              [".Dt ", progNameChunk progname, " 1"],
              -- Operating system footer
              [".Os"],
              -- Section header
              [".Sh ", "NAME"],
              [".Nm ", progNameChunk progname],
              [".Nd ", chunk $ T.pack progDesc],
              [".Sh ", "VERSION"],
              [versionChunk version],
              [".Sh ", "SYNOPSIS"],
              renderShortOptDocs progname optDocs,
              [".Sh ", "SETTINGS"],
              renderSetDocs docs
            ],
            concat
              [ [ [".Sh ", "COMMANDS"],
                  renderCommandDocs docs
                ]
                | not (null commandDocs)
              ],
            concat
              [ [ [".Sh ", "OPTIONS"],
                  renderLongOptDocs optDocs
                ]
                | not (nullDocs optDocs)
              ],
            concat
              [ [ [".Sh ", "ENVIRONMENT VARIABLES"],
                  renderEnvDocs envDocs
                ]
                | not (nullDocs envDocs)
              ],
            concat
              [ [ [".Sh ", "CONFIGURATION VALUES"],
                  renderConfDocs confDocs
                ]
                | not (nullDocs confDocs)
              ]
          ]

-- | Render reference documentation
renderReferenceDocumentation :: String -> AnyDocs SetDoc -> [Chunk]
renderReferenceDocumentation progname docs =
  let optDocs = docsToOptDocs docs
      envDocs = docsToEnvDocs docs
      confDocs = docsToConfDocs docs
      commandDocs = docsToCommandDocs docs
   in unlinesChunks $
        concat
          [ [ usageChunk : renderShortOptDocs progname optDocs,
              [],
              headerChunks "All settings",
              renderSetDocs docs
            ],
            concat
              [ [ headerChunks "All commands",
                  renderCommandDocs docs
                ]
                | not (null commandDocs)
              ],
            concat
              [ [ headerChunks "Options",
                  renderLongOptDocs optDocs
                ]
                | not (nullDocs optDocs)
              ],
            concat
              [ [ headerChunks "Environment Variables",
                  renderEnvDocs envDocs
                ]
                | not (nullDocs envDocs)
              ],
            concat
              [ [ headerChunks "Configuration Values",
                  renderConfDocs confDocs
                ]
                | not (nullDocs confDocs)
              ]
          ]

nullDocs :: AnyDocs a -> Bool
nullDocs = \case
  AnyDocsCommands cs -> all nullCommandDoc cs
  AnyDocsOr [] -> True
  AnyDocsOr _ -> False
  AnyDocsAnd [] -> True
  AnyDocsAnd _ -> False
  AnyDocsSingle _ -> False
  where
    nullCommandDoc :: CommandDoc a -> Bool
    nullCommandDoc = nullDocs . commandDocs

-- | Render the output of @--version@
renderVersionPage :: String -> Version -> [Chunk]
renderVersionPage progname version =
  unwordsChunks
    [ [progNameChunk progname],
      [versionChunk version],
      ["\n"]
    ]

-- | Render the output of @--help@
renderHelpPage :: String -> String -> AnyDocs SetDoc -> [Chunk]
renderHelpPage progname progDesc docs =
  unlinesChunks $
    concat
      [ [ usageChunk : renderShortOptDocs progname (docsToOptDocs docs),
          [],
          unlinesChunks $ progDescLines progDesc
        ],
        concat
          [ [ headerChunks "Available settings",
              renderSetDocs docs
            ]
            | not (nullDocs docs)
          ],
        concat
          [ [ headerChunks "Available commands",
              renderCommandDocsShort docs
            ]
            | not (null (docsToCommandDocs docs))
          ]
      ]

renderCommandHelpPage :: String -> [String] -> CommandDoc SetDoc -> [Chunk]
renderCommandHelpPage progname commandPath CommandDoc {..} =
  renderHelpPage (unwords $ progname : commandPath ++ [commandDocArgument]) commandDocHelp commandDocs

renderSetDocs :: AnyDocs SetDoc -> [Chunk]
renderSetDocs = unlinesChunks . go
  where
    go :: AnyDocs SetDoc -> [[Chunk]]
    go = \case
      AnyDocsCommands _ -> []
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> goOr ds
      AnyDocsSingle d -> indent (renderSetDoc d)

    -- Group together settings with the same help (produced by combinators like enableDisableSwitch)
    goOr :: [AnyDocs SetDoc] -> [[Chunk]]
    goOr = \case
      [] -> []
      [d] -> go d
      (AnyDocsSingle d : ds) ->
        case setDocHelp d of
          Nothing -> go (AnyDocsSingle d) ++ goOr ds
          Just h ->
            let (sds, rest) = goSameHelp h ds
             in concat
                  [ indent $ renderSetDocHeader (Just h),
                    indent $ concatMap renderSetDocWithoutHeader $ d : sds,
                    [[] | not (null rest)],
                    goOr rest
                  ]
      (d : ds) -> go d ++ goOr ds

    goSameHelp :: Help -> [AnyDocs SetDoc] -> ([SetDoc], [AnyDocs SetDoc])
    goSameHelp h = \case
      [] -> ([], [])
      (AnyDocsSingle d : ds) ->
        if setDocHelp d == Just h
          then
            let (sds, rest) = goSameHelp h ds
             in (d : sds, rest)
          else ([], AnyDocsSingle d : ds)
      ds -> ([], ds)

renderCommandDocs :: AnyDocs SetDoc -> [Chunk]
renderCommandDocs = unlinesChunks . go True
  where
    go :: Bool -> AnyDocs SetDoc -> [[Chunk]]
    go isTopLevel = \case
      AnyDocsCommands cs -> concatMap goCommand cs
      AnyDocsAnd ds -> concatMap (go isTopLevel) ds
      AnyDocsOr ds -> goOr isTopLevel ds
      AnyDocsSingle d
        | isTopLevel -> []
        | otherwise -> indent (renderSetDoc d)

    goCommand :: CommandDoc SetDoc -> [[Chunk]]
    goCommand CommandDoc {..} =
      indent $
        [helpChunk commandDocHelp]
          : ["command: ", commandChunk commandDocArgument]
          : go False commandDocs
          ++ [[]]

    -- Group together settings with the same help (produced by combinators like enableDisableSwitch)
    goOr :: Bool -> [AnyDocs SetDoc] -> [[Chunk]]
    goOr isTopLevel = \case
      [] -> []
      [d] -> go isTopLevel d
      (AnyDocsSingle d : ds) ->
        case setDocHelp d of
          Nothing -> go isTopLevel (AnyDocsSingle d) ++ goOr isTopLevel ds
          Just h ->
            let (sds, rest) = goSameHelp h ds
             in concat
                  [ concat
                      [ concat
                          [ indent $ renderSetDocHeader (Just h),
                            indent $ concatMap renderSetDocWithoutHeader $ d : sds,
                            [[]]
                          ]
                        | not isTopLevel
                      ],
                    goOr isTopLevel rest
                  ]
      (d : ds) -> go isTopLevel d ++ goOr isTopLevel ds
    goSameHelp :: Help -> [AnyDocs SetDoc] -> ([SetDoc], [AnyDocs SetDoc])
    goSameHelp h = \case
      [] -> ([], [])
      (AnyDocsSingle d : ds) ->
        if setDocHelp d == Just h
          then
            let (sds, rest) = goSameHelp h ds
             in (d : sds, rest)
          else ([], AnyDocsSingle d : ds)
      ds -> ([], ds)

renderCommandDocsShort :: AnyDocs SetDoc -> [Chunk]
renderCommandDocsShort = layoutAsTable . go
  where
    go :: AnyDocs SetDoc -> [[[Chunk]]]
    go = \case
      AnyDocsCommands cs -> concatMap goCommand cs
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle _ -> []

    goCommand :: CommandDoc SetDoc -> [[[Chunk]]]
    goCommand CommandDoc {..} =
      [indent [[commandChunk commandDocArgument], [helpChunk commandDocHelp]]]

parserOptDocs :: Parser a -> AnyDocs OptDoc
parserOptDocs = docsToOptDocs . parserDocs

docsToOptDocs :: AnyDocs SetDoc -> AnyDocs OptDoc
docsToOptDocs = mapMaybeDocs setDocOptDoc

setDocOptDoc :: SetDoc -> Maybe OptDoc
setDocOptDoc SetDoc {..} = do
  guard $ setDocTryArgument || setDocTrySwitch || setDocTryOption
  let optDocTryArgument = setDocTryArgument
      optDocTrySwitch = setDocTrySwitch
      optDocTryOption = setDocTryOption
      optDocDasheds = setDocDasheds
      optDocDefault = setDocDefault
      optDocExamples = setDocExamples
      optDocMetavar = setDocMetavar
      optDocHelp = setDocHelp
  pure OptDoc {..}

-- | Render short-form documentation of options
renderShortOptDocs :: String -> AnyDocs OptDoc -> [Chunk]
renderShortOptDocs progname = unwordsChunks . (\cs -> [[progNameChunk progname], cs]) . go
  where
    go :: AnyDocs OptDoc -> [Chunk]
    go = \case
      AnyDocsCommands _ -> ["COMMAND"]
      AnyDocsAnd ds -> unwordsChunks $ map go ds
      AnyDocsOr ds -> renderOrChunks $ map go ds
      AnyDocsSingle OptDoc {..} ->
        unwordsChunks $
          concat
            [ [ [mMetavarChunk optDocMetavar]
                | optDocTryArgument
              ],
              [ concat $ maybeToList $ dashedChunks optDocDasheds
                | optDocTrySwitch
              ],
              [ concat
                  [ concat $ maybeToList $ dashedChunks optDocDasheds,
                    [" ", mMetavarChunk optDocMetavar]
                  ]
                | optDocTryOption
              ]
            ]

renderOrChunks :: [[Chunk]] -> [Chunk]
renderOrChunks os =
  unwordsChunks $
    intersperse [orChunk] $
      map parenthesise os
  where
    parenthesise :: [Chunk] -> [Chunk]
    parenthesise [c] = [c]
    parenthesise cs = fore cyan "(" : cs ++ [fore cyan ")"]

orChunk :: Chunk
orChunk = fore cyan "|"

-- | Render long-form documentation of options
renderLongOptDocs :: AnyDocs OptDoc -> [Chunk]
renderLongOptDocs = unlinesChunks . go
  where
    go :: AnyDocs OptDoc -> [[Chunk]]
    go = \case
      AnyDocsCommands cs ->
        concatMap
          ( \CommandDoc {..} ->
              indent $
                unwordsChunks [[commandChunk commandDocArgument], [helpChunk commandDocHelp]]
                  : indent (go commandDocs)
          )
          cs
      AnyDocsAnd ds -> case goTable (AnyDocsAnd ds) of
        Nothing -> concatMap go ds
        Just csss -> indent $ layoutAsTableLines csss
      AnyDocsOr ds -> case goTable (AnyDocsOr ds) of
        Nothing -> concatMap go ds
        Just csss -> indent $ layoutAsTableLines csss
      AnyDocsSingle vs -> indent $ layoutAsTableLines [renderOptDocLong vs]

    goTable :: AnyDocs OptDoc -> Maybe [[[Chunk]]]
    goTable = \case
      AnyDocsCommands _ -> Nothing
      AnyDocsAnd ds -> concat <$> mapM goTable ds
      AnyDocsOr ds -> concat <$> mapM goTable ds
      AnyDocsSingle od -> Just [renderOptDocLong od]

renderOptDocLong :: OptDoc -> [[Chunk]]
renderOptDocLong OptDoc {..} =
  concat
    [ [ unwordsChunks $
          concat
            [ maybeToList $ dashedChunks optDocDasheds,
              [ [ mMetavarChunk optDocMetavar
                ]
                | optDocTryArgument
              ]
            ],
        [mHelpChunk optDocHelp]
      ],
      [defaultValueChunks d | d <- maybeToList optDocDefault],
      [exampleValuesChunks optDocExamples | not (null optDocExamples)]
    ]

parserEnvDocs :: Parser a -> AnyDocs EnvDoc
parserEnvDocs = docsToEnvDocs . parserDocs

docsToEnvDocs :: AnyDocs SetDoc -> AnyDocs EnvDoc
docsToEnvDocs = mapMaybeDocs setDocEnvDoc

setDocEnvDoc :: SetDoc -> Maybe EnvDoc
setDocEnvDoc SetDoc {..} = do
  envDocVars <- setDocEnvVars
  let envDocDefault = setDocDefault
  let envDocExamples = setDocExamples
  let envDocMetavar = setDocMetavar
  let envDocHelp = setDocHelp
  pure EnvDoc {..}

settingEnvDoc :: Setting a -> Maybe EnvDoc
settingEnvDoc = settingSetDoc >=> setDocEnvDoc

-- | Render documentation of envionment variables
renderEnvDocs :: AnyDocs EnvDoc -> [Chunk]
renderEnvDocs = layoutAsTable . go
  where
    go :: AnyDocs EnvDoc -> [[[Chunk]]]
    go = \case
      AnyDocsCommands cs -> concatMap (go . commandDocs) cs
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> [indent $ renderEnvDoc ed]

renderEnvDoc :: EnvDoc -> [[Chunk]]
renderEnvDoc EnvDoc {..} =
  concat
    [ [ unwordsChunks
          [ envVarChunksNE envDocVars,
            [ mMetavarChunk envDocMetavar
            ]
          ],
        [mHelpChunk envDocHelp]
      ],
      [defaultValueChunks d | d <- maybeToList envDocDefault],
      [exampleValuesChunks envDocExamples | not (null envDocExamples)]
    ]

parserConfDocs :: Parser a -> AnyDocs ConfDoc
parserConfDocs = docsToConfDocs . parserDocs

docsToConfDocs :: AnyDocs SetDoc -> AnyDocs ConfDoc
docsToConfDocs = mapMaybeDocs setDocConfDoc

setDocConfDoc :: SetDoc -> Maybe ConfDoc
setDocConfDoc SetDoc {..} = do
  confDocKeys <- setDocConfKeys
  let confDocDefault = setDocDefault
  let confDocExamples = setDocExamples
  let confDocHelp = setDocHelp
  pure ConfDoc {..}

settingConfDoc :: Setting a -> Maybe ConfDoc
settingConfDoc = settingSetDoc >=> setDocConfDoc

docsToCommandDocs :: AnyDocs SetDoc -> [CommandDoc SetDoc]
docsToCommandDocs = \case
  AnyDocsCommands cs -> cs
  AnyDocsAnd ds -> concatMap docsToCommandDocs ds
  AnyDocsOr ds -> concatMap docsToCommandDocs ds
  AnyDocsSingle _ -> []

-- | Render documentation of configuration values
renderConfDocs :: AnyDocs ConfDoc -> [Chunk]
renderConfDocs = unlinesChunks . go
  where
    go :: AnyDocs ConfDoc -> [[Chunk]]
    go = \case
      AnyDocsCommands cs -> concatMap (go . commandDocs) cs
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> indent (renderConfDoc ed)

renderConfDoc :: ConfDoc -> [[Chunk]]
renderConfDoc ConfDoc {..} =
  concat
    [ [[mHelpChunk confDocHelp]],
      [defaultValueChunks d | d <- maybeToList confDocDefault],
      [exampleValuesChunks confDocExamples | not (null confDocExamples)],
      concatMap
        ( \(key, schema) ->
            case jsonSchemaChunkLines schema of
              [line] ->
                [[confValChunk key, ": "] ++ line]
              ls ->
                [confValChunk key, ":"] : indent ls
        )
        (NE.toList confDocKeys)
    ]
