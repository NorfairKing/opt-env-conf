{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Doc
  ( renderVersionPage,
    parserDocs,
    renderHelpPage,
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
import Control.Arrow
import Control.Monad
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import OptEnvConf.Args (Dashed (..))
import qualified OptEnvConf.Args as Args
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
    optDocMetavar :: !(Maybe Metavar),
    optDocHelp :: !(Maybe String)
  }
  deriving (Show)

data EnvDoc = EnvDoc
  { envDocVars :: !(NonEmpty String),
    envDocDefault :: !(Maybe String),
    envDocMetavar :: !(Maybe Metavar),
    envDocHelp :: !(Maybe String)
  }
  deriving (Show)

data ConfDoc = ConfDoc
  { confDocKeys :: !(NonEmpty (NonEmpty String, JSONSchema)),
    confDocDefault :: !(Maybe String),
    confDocHelp :: !(Maybe String)
  }
  deriving (Show)

data AnyDocs a
  = AnyDocsCommands [CommandDoc a]
  | AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle !a
  deriving (Show)

instance Functor AnyDocs where
  fmap f = \case
    AnyDocsCommands cs -> AnyDocsCommands $ map (fmap f) cs
    AnyDocsAnd as -> AnyDocsAnd $ fmap (fmap f) as
    AnyDocsOr as -> AnyDocsOr $ fmap (fmap f) as
    AnyDocsSingle a -> AnyDocsSingle $ f a

instance Foldable AnyDocs where
  foldMap f = \case
    AnyDocsCommands cs -> foldMap (foldMap f) cs
    AnyDocsAnd as -> foldMap (foldMap f) as
    AnyDocsOr as -> foldMap (foldMap f) as
    AnyDocsSingle a -> f a

instance Traversable AnyDocs where
  traverse f = \case
    AnyDocsCommands cs -> AnyDocsCommands <$> traverse (traverse f) cs
    AnyDocsAnd as -> AnyDocsAnd <$> traverse (traverse f) as
    AnyDocsOr as -> AnyDocsOr <$> traverse (traverse f) as
    AnyDocsSingle a -> AnyDocsSingle <$> f a

data CommandDoc a = CommandDoc
  { commandDocArgument :: String,
    commandDocHelp :: Help,
    commandDocs :: AnyDocs a
  }
  deriving (Show)

instance Functor CommandDoc where
  fmap f cd = cd {commandDocs = fmap f (commandDocs cd)}

instance Foldable CommandDoc where
  foldMap f = foldMap f . commandDocs

instance Traversable CommandDoc where
  traverse f cd = (\d -> cd {commandDocs = d}) <$> traverse f (commandDocs cd)

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
      ParserAllOrNothing _ p -> go p
      ParserCheck _ _ _ p -> go p
      ParserCommands _ cs -> AnyDocsCommands $ map goCommand cs
      ParserWithConfig p1 p2 -> AnyDocsAnd [go p1, go p2] -- TODO: is this right? Maybe we want to document that it's not a pure parser?
      ParserSetting _ set -> maybe noDocs AnyDocsSingle $ settingSetDoc set
    goCommand :: Command a -> CommandDoc SetDoc
    goCommand Command {..} =
      CommandDoc
        { commandDocArgument = commandArg,
          commandDocHelp = commandHelp,
          commandDocs = go commandParser
        }

settingSetDoc :: Setting a -> Maybe SetDoc
settingSetDoc Setting {..} = do
  guard $ not settingHidden
  let setDocDasheds = settingDasheds
  let setDocTryArgument = settingTryArgument
  let setDocTrySwitch = isJust settingSwitchValue
  let setDocTryOption = settingTryOption
  let setDocEnvVars = settingEnvVars
  let setDocConfKeys = NE.map (second (\(DecodingCodec c) -> jsonSchemaVia c)) <$> settingConfigVals
  let setDocDefault = snd <$> settingDefaultValue
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
    [ [ unwordsChunks
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

stringLines :: String -> [[Chunk]]
stringLines s =
  let ls = T.lines (T.pack s)
   in map (pure . chunk) ls

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
              renderSetDocs docs,
              [".Sh ", "COMMANDS"],
              renderCommandDocs docs
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
   in unlinesChunks $
        concat
          [ [ usageChunk : renderShortOptDocs progname optDocs,
              [],
              headerChunks "All settings",
              renderSetDocs docs,
              headerChunks "All commands",
              renderCommandDocs docs
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
  unlinesChunks
    [ usageChunk : renderShortOptDocs progname (docsToOptDocs docs),
      [],
      unlinesChunks $ progDescLines progDesc,
      headerChunks "Available settings",
      renderSetDocs docs,
      headerChunks "Available commands",
      renderCommandDocs docs
    ]

renderSetDocs :: AnyDocs SetDoc -> [Chunk]
renderSetDocs = unlinesChunks . go
  where
    go :: AnyDocs SetDoc -> [[Chunk]]
    go = \case
      AnyDocsCommands _ -> [] -- todo: this empty list causes the double newline between settings and commands
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
                    [[]],
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
renderCommandDocs = unlinesChunks . (\cs -> if null cs then [["no commands available"]] else cs) . go
  where
    go :: AnyDocs SetDoc -> [[Chunk]]
    go = \case
      AnyDocsCommands cs ->
        let maxLength = maximum $ length . commandDocArgument <$> cs
         in concatMap (goCommand maxLength) cs
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> goOr ds
      AnyDocsSingle _ -> []

    goCommand :: Int -> CommandDoc SetDoc -> [[Chunk]]
    goCommand maxLength CommandDoc {..} =
      indent $
        if length commandDocArgument <= maxLength' - 1
          then [commandChunk commandDocArgument, chunk (T.replicate (maxLength' - length commandDocArgument) " "), helpChunk commandDocHelp] : []
          else [commandChunk commandDocArgument] : [[chunk (T.replicate maxLength' " "), helpChunk commandDocHelp]]
      where
        maxLength' = minimum [25, maximum [10, maxLength]]

    -- Group together settings with the same help (produced by combinators like enableDisableSwitch)
    goOr :: [AnyDocs SetDoc] -> [[Chunk]]
    goOr = \case
      [] -> []
      [d] -> go d
      (AnyDocsSingle d : ds) ->
        case setDocHelp d of
          Nothing -> go (AnyDocsSingle d) ++ goOr ds
          Just h ->
            let (_, rest) = goSameHelp h ds
             in concat
                  [  goOr rest
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
      optDocMetavar = setDocMetavar
      optDocHelp = setDocHelp
  pure OptDoc {..}

-- | Render short-form documentation of options
renderShortOptDocs :: String -> AnyDocs OptDoc -> [Chunk]
renderShortOptDocs progname = unwordsChunks . (\cs -> [[progNameChunk progname], cs]) . go 0
  where
    go :: Int -> AnyDocs OptDoc -> [Chunk]
    go nestingLevel = \case
      AnyDocsCommands _ -> unwordsChunks $ [["COMMAND"]]
      AnyDocsAnd ds -> unwordsChunks $ map (go (nestingLevel + 1)) ds
      AnyDocsOr ds -> renderOrChunks $ map (go (nestingLevel + 1)) ds
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
    intersperse [orChunkNewline] $
      map parenthesise os
  where
    parenthesise :: [Chunk] -> [Chunk]
    parenthesise [c] = [c]
    parenthesise cs = fore cyan "(" : cs ++ [fore cyan ")"]

orChunkNewline :: Chunk
orChunkNewline = fore cyan "\n |"

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
  [ unwordsChunks $
      concat
        [ maybeToList $ dashedChunks optDocDasheds,
          [ [ mMetavarChunk optDocMetavar
            ]
            | optDocTryArgument
          ]
        ],
    [mHelpChunk optDocHelp],
    unwordsChunks [defaultValueChunks d | d <- maybeToList optDocDefault]
  ]

parserEnvDocs :: Parser a -> AnyDocs EnvDoc
parserEnvDocs = docsToEnvDocs . parserDocs

docsToEnvDocs :: AnyDocs SetDoc -> AnyDocs EnvDoc
docsToEnvDocs = mapMaybeDocs setDocEnvDoc

setDocEnvDoc :: SetDoc -> Maybe EnvDoc
setDocEnvDoc SetDoc {..} = do
  envDocVars <- setDocEnvVars
  let envDocDefault = setDocDefault
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
  [ unwordsChunks
      [ envVarChunksNE envDocVars,
        [ mMetavarChunk envDocMetavar
        ]
      ],
    [mHelpChunk envDocHelp]
  ]

parserConfDocs :: Parser a -> AnyDocs ConfDoc
parserConfDocs = docsToConfDocs . parserDocs

docsToConfDocs :: AnyDocs SetDoc -> AnyDocs ConfDoc
docsToConfDocs = mapMaybeDocs setDocConfDoc

setDocConfDoc :: SetDoc -> Maybe ConfDoc
setDocConfDoc SetDoc {..} = do
  confDocKeys <- setDocConfKeys
  let confDocDefault = setDocDefault
  let confDocHelp = setDocHelp
  pure ConfDoc {..}

settingConfDoc :: Setting a -> Maybe ConfDoc
settingConfDoc = settingSetDoc >=> setDocConfDoc

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
  [mHelpChunk confDocHelp]
    : concatMap
      ( \(key, schema) ->
          case jsonSchemaChunkLines schema of
            [line] ->
              [[confValChunk key, ": "] ++ line]
            ls ->
              [confValChunk key, ":"] : indent ls
      )
      (NE.toList confDocKeys)

progNameChunk :: String -> Chunk
progNameChunk = fore yellow . chunk . T.pack

versionChunk :: Version -> Chunk
versionChunk = chunk . T.pack . showVersion

usageChunk :: Chunk
usageChunk = fore cyan "Usage: "

commandChunk :: String -> Chunk
commandChunk = fore magenta . chunk . T.pack

mMetavarChunk :: Maybe Metavar -> Chunk
mMetavarChunk = metavarChunk . fromMaybe "METAVAR"

metavarChunk :: Metavar -> Chunk
metavarChunk = fore yellow . chunk . T.pack

dashedChunks :: [Dashed] -> Maybe [Chunk]
dashedChunks = fmap dashedChunksNE . NE.nonEmpty

dashedChunksNE :: NonEmpty Dashed -> [Chunk]
dashedChunksNE = intersperse (fore cyan "|") . map dashedChunk . NE.toList

dashedChunk :: Dashed -> Chunk
dashedChunk = fore white . chunk . T.pack . Args.renderDashed

envVarChunksNE :: NonEmpty String -> [Chunk]
envVarChunksNE = intersperse (fore cyan "|") . map envVarChunk . NE.toList

envVarChunk :: String -> Chunk
envVarChunk = fore white . chunk . T.pack

confValChunk :: NonEmpty String -> Chunk
confValChunk = fore white . chunk . T.pack . intercalate "." . NE.toList

defaultValueChunks :: String -> [Chunk]
defaultValueChunks val = ["default: ", fore yellow $ chunk $ T.pack val]

mHelpChunk :: Maybe Help -> Chunk
mHelpChunk = maybe (fore red "undocumented") helpChunk

helpChunk :: Help -> Chunk
helpChunk = fore blue . chunk . T.pack

headerChunks :: Text -> [Chunk]
headerChunks t = [fore cyan (chunk t), ":"]

indent :: [[Chunk]] -> [[Chunk]]
indent = map ("  " :)
