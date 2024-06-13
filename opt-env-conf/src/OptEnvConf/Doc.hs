{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Doc where

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
import OptEnvConf.ArgMap (Dashed (..))
import qualified OptEnvConf.ArgMap as AM
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
  deriving (Show, Eq)

data OptDoc = OptDoc
  { optDocTryArgument :: !Bool,
    optDocTrySwitch :: !Bool,
    optDocTryOption :: !Bool,
    optDocDasheds :: ![Dashed],
    optDocDefault :: !(Maybe String),
    optDocMetavar :: !(Maybe Metavar),
    optDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data EnvDoc = EnvDoc
  { envDocVars :: !(NonEmpty String),
    envDocDefault :: !(Maybe String),
    envDocMetavar :: !(Maybe Metavar),
    envDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data ConfDoc = ConfDoc
  { confDocKeys :: !(NonEmpty (NonEmpty String, JSONSchema)),
    confDocDefault :: !(Maybe String),
    confDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data AnyDocs a
  = AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle !a
  deriving (Show, Eq)

instance Functor AnyDocs where
  fmap f = \case
    AnyDocsAnd as -> AnyDocsAnd $ fmap (fmap f) as
    AnyDocsOr as -> AnyDocsOr $ fmap (fmap f) as
    AnyDocsSingle a -> AnyDocsSingle $ f a

instance Foldable AnyDocs where
  foldMap f = \case
    AnyDocsAnd as -> foldMap (foldMap f) as
    AnyDocsOr as -> foldMap (foldMap f) as
    AnyDocsSingle a -> f a

instance Traversable AnyDocs where
  traverse f = \case
    AnyDocsAnd as -> AnyDocsAnd <$> traverse (traverse f) as
    AnyDocsOr as -> AnyDocsOr <$> traverse (traverse f) as
    AnyDocsSingle a -> AnyDocsSingle <$> f a

mapMaybeDocs :: (a -> Maybe b) -> AnyDocs a -> AnyDocs b
mapMaybeDocs func = simplifyAnyDocs . go
  where
    go = \case
      AnyDocsAnd ds -> AnyDocsAnd $ map go ds
      AnyDocsOr ds -> AnyDocsOr $ map go ds
      AnyDocsSingle d -> maybe (AnyDocsAnd []) AnyDocsSingle $ func d

simplifyAnyDocs :: AnyDocs a -> AnyDocs a
simplifyAnyDocs = go
  where
    go = \case
      AnyDocsAnd ds -> case concatMap goAnd ds of
        [a] -> a
        as -> AnyDocsAnd as
      AnyDocsOr ds -> case concatMap goOr ds of
        [a] -> a
        as -> AnyDocsOr as
      AnyDocsSingle v -> AnyDocsSingle v

    goAnd = \case
      AnyDocsAnd ds -> concatMap (goAnd . go) ds
      AnyDocsOr [] -> []
      ds -> [go ds]

    goOr = \case
      AnyDocsOr ds -> concatMap (goOr . go) ds
      AnyDocsAnd [] -> []
      ds -> [go ds]

parserDocs :: Parser a -> AnyDocs SetDoc
parserDocs = simplifyAnyDocs . go
  where
    noDocs = AnyDocsAnd []
    go :: Parser a -> AnyDocs SetDoc
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> noDocs
      ParserAp pf pa -> AnyDocsAnd [go pf, go pa]
      ParserSelect p1 p2 -> AnyDocsAnd [go p1, go p2]
      ParserEmpty -> AnyDocsOr []
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserMany p -> go p -- TODO: is this right?
      ParserMapIO _ p -> go p -- TODO: is this right? Maybe we want to document that it's not a pure parser?
      ParserWithConfig p1 p2 -> AnyDocsAnd [go p1, go p2] -- TODO: is this right? Maybe we want to document that it's not a pure parser?
      ParserSetting set -> maybe noDocs AnyDocsSingle $ settingSetDoc set

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
            [metavarChunk $ fromMaybe "ARG" setDocMetavar]
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
              ++ [" ", metavarChunk $ fromMaybe "ARG" setDocMetavar]
          ]
        | setDocTryOption,
          dasheds <- maybeToList (NE.nonEmpty setDocDasheds)
      ],
      [ unwordsChunks
          [ ["env:"],
            envVarChunksNE vars
              ++ [" ", metavarChunk $ fromMaybe "ARG" setDocMetavar]
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
                    ["config: ", confValChunk key, ":"]
                      : indent ls
            )
            (NE.toList confs)
          | confs <- maybeToList setDocConfKeys
        ]
    ]

helpLines :: Help -> [[Chunk]]
helpLines h =
  let ls = T.lines (T.pack h)
   in map ((: []) . fore blue . chunk) ls

renderManPage :: String -> AnyDocs SetDoc -> [Chunk]
renderManPage progname docs =
  let optDocs = docsToOptDocs docs
      envDocs = docsToEnvDocs docs
      confDocs = docsToConfDocs docs
   in unlinesChunks $
        concat
          [ [ renderShortOptDocs progname optDocs,
              [],
              headerChunks "All settings",
              renderAnyDocs docs
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
  AnyDocsOr [] -> True
  AnyDocsOr _ -> False
  AnyDocsAnd [] -> True
  AnyDocsAnd _ -> False
  AnyDocsSingle _ -> False

renderHelpPage :: String -> AnyDocs SetDoc -> [Chunk]
renderHelpPage progname docs =
  unlinesChunks
    [ renderShortOptDocs progname (docsToOptDocs docs),
      [],
      renderAnyDocs docs
    ]

renderAnyDocs :: AnyDocs SetDoc -> [Chunk]
renderAnyDocs = unlinesChunks . go
  where
    go :: AnyDocs SetDoc -> [[Chunk]]
    go = \case
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

renderShortOptDocs :: String -> AnyDocs OptDoc -> [Chunk]
renderShortOptDocs progname = unwordsChunks . (\cs -> [[fore cyan "Usage: ", fore yellow (chunk (T.pack progname))], cs]) . go
  where
    go :: AnyDocs OptDoc -> [Chunk]
    go = \case
      AnyDocsAnd ds -> unwordsChunks $ map go ds
      AnyDocsOr ds -> renderOrChunks (map go ds)
      AnyDocsSingle OptDoc {..} ->
        unwordsChunks $
          concat
            [ [ [metavarChunk $ fromMaybe "ARG" optDocMetavar]
                | optDocTryArgument
              ],
              [ concat $ maybeToList $ dashedChunks optDocDasheds
                | optDocTrySwitch
              ],
              [ concat
                  [ concat $ maybeToList $ dashedChunks optDocDasheds,
                    [" ", metavarChunk $ fromMaybe "ARG" optDocMetavar]
                  ]
                | optDocTryOption
              ]
            ]

renderOrChunks :: [[Chunk]] -> [Chunk]
renderOrChunks os =
  unwordsChunks $
    intersperse [fore cyan "|"] $
      map parenthesise os
  where
    parenthesise :: [Chunk] -> [Chunk]
    parenthesise cs = fore cyan "(" : cs ++ [fore cyan ")"]

renderLongOptDocs :: AnyDocs OptDoc -> [Chunk]
renderLongOptDocs = layoutAsTable . go
  where
    go :: AnyDocs OptDoc -> [[[Chunk]]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> [indent $ renderOptDocLong vs]

renderOptDocLong :: OptDoc -> [[Chunk]]
renderOptDocLong OptDoc {..} =
  [ unwordsChunks $
      concat
        [ maybeToList $ dashedChunks optDocDasheds,
          [ [ metavarChunk $ fromMaybe "ARG" optDocMetavar
            ]
            | optDocTryArgument
          ]
        ],
    [helpChunk optDocHelp],
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

renderEnvDocs :: AnyDocs EnvDoc -> [Chunk]
renderEnvDocs = layoutAsTable . go
  where
    go :: AnyDocs EnvDoc -> [[[Chunk]]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> [indent $ renderEnvDoc ed]

renderEnvDoc :: EnvDoc -> [[Chunk]]
renderEnvDoc EnvDoc {..} =
  [ unwordsChunks
      [ envVarChunksNE envDocVars,
        [ metavarChunk $ fromMaybe "ARG" envDocMetavar
        ]
      ],
    [helpChunk envDocHelp]
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

renderConfDocs :: AnyDocs ConfDoc -> [Chunk]
renderConfDocs = unlinesChunks . go
  where
    go :: AnyDocs ConfDoc -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> indent (renderConfDoc ed)

renderConfDoc :: ConfDoc -> [[Chunk]]
renderConfDoc ConfDoc {..} =
  [helpChunk confDocHelp]
    : concatMap
      ( \(key, schema) ->
          case jsonSchemaChunkLines schema of
            [line] ->
              [[confValChunk key, ": "] ++ line]
            ls ->
              [confValChunk key, ":"] : indent ls
      )
      (NE.toList confDocKeys)

metavarChunk :: Metavar -> Chunk
metavarChunk = fore yellow . chunk . T.pack

dashedChunks :: [Dashed] -> Maybe [Chunk]
dashedChunks = fmap dashedChunksNE . NE.nonEmpty

dashedChunksNE :: NonEmpty Dashed -> [Chunk]
dashedChunksNE = intersperse (fore cyan "|") . map dashedChunk . NE.toList

dashedChunk :: Dashed -> Chunk
dashedChunk = fore white . chunk . T.pack . AM.renderDashed

envVarChunks :: Maybe (NonEmpty String) -> Maybe [Chunk]
envVarChunks = fmap envVarChunksNE

envVarChunksNE :: NonEmpty String -> [Chunk]
envVarChunksNE = intersperse (fore cyan "|") . map envVarChunk . NE.toList

envVarChunk :: String -> Chunk
envVarChunk = fore white . chunk . T.pack

confValChunk :: NonEmpty String -> Chunk
confValChunk = fore white . chunk . T.pack . intercalate "." . NE.toList

defaultValueChunks :: String -> [Chunk]
defaultValueChunks val = ["default: ", fore yellow $ chunk $ T.pack val]

helpChunk :: Maybe Help -> Chunk
helpChunk = maybe (fore red "!! undocumented !!") (fore blue . chunk . T.pack)

headerChunks :: Text -> [Chunk]
headerChunks t = [fore cyan (chunk t), ":"]

indent :: [[Chunk]] -> [[Chunk]]
indent = map ("  " :)
