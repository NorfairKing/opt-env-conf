{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Doc where

import Autodocodec.Schema
import Autodocodec.Yaml.Schema
import Control.Arrow (second)
import Control.Monad
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
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
    setDocConfKeys :: !(Maybe (NonEmpty (String, JSONSchema))),
    setDocMetavar :: !(Maybe Metavar),
    setDocHelp :: !(Maybe String)
  }
  deriving (Show)

data OptDoc = OptDoc
  { optDocTryArgument :: !Bool,
    optDocTrySwitch :: !Bool,
    optDocTryOption :: !Bool,
    optDocDasheds :: ![Dashed],
    optDocMetavar :: !(Maybe Metavar),
    optDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data EnvDoc = EnvDoc
  { envDocVars :: !(NonEmpty String),
    envDocMetavar :: !(Maybe Metavar),
    envDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data ConfDoc = ConfDoc
  { confDocKeys :: !(NonEmpty (String, JSONSchema)),
    confDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data AnyDocs a
  = AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle !a
  deriving (Show)

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
      ParserEmpty -> noDocs
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserMany p -> go p -- TODO: is this right?
      ParserSome p -> go p -- TODO: is this right?
      ParserMapIO _ p -> go p -- TODO: is this right? Maybe we want to document that it's not a pure parser?
      ParserWithConfig p1 p2 -> AnyDocsAnd [go p1, go p2] -- TODO: is this right? Maybe we want to document that it's not a pure parser?
      ParserOptionalFirst ps -> AnyDocsOr $ map go ps
      ParserRequiredFirst ps -> AnyDocsOr $ map go ps
      ParserPrefixed prefix p -> setDocPrefixed prefix <$> go p
      ParserSetting set -> AnyDocsSingle $ settingSetDoc set

settingSetDoc :: Setting a -> SetDoc
settingSetDoc Setting {..} =
  let setDocDasheds = settingDasheds
      setDocTryArgument = settingTryArgument
      setDocTrySwitch = isJust settingSwitchValue
      setDocTryOption = settingTryOption
      setDocEnvVars = settingEnvVars
      setDocConfKeys = NE.map (second jsonSchemaVia) <$> settingConfigVals
      setDocMetavar = settingMetavar
      setDocHelp = settingHelp
   in SetDoc {..}

settingOptDoc :: Setting a -> Maybe OptDoc
settingOptDoc = setDocOptDoc . settingSetDoc

setDocPrefixed :: String -> SetDoc -> SetDoc
setDocPrefixed prefix sd =
  sd {setDocEnvVars = NE.map (prefix <>) <$> setDocEnvVars sd}

renderSetDoc :: SetDoc -> [[[Chunk]]]
renderSetDoc SetDoc {..} =
  addHelpText $
    concat
      [ [ [ ["argument:"],
            [metavarChunk $ fromMaybe "ARG" setDocMetavar]
          ]
          | setDocTryArgument
        ],
        [ [ ["switch:"],
            dashedChunksNE dasheds
          ]
          | setDocTrySwitch,
            dasheds <- maybeToList (NE.nonEmpty setDocDasheds)
        ],
        [ [ ["option:"],
            dashedChunksNE dasheds
              ++ [" ", metavarChunk $ fromMaybe "ARG" setDocMetavar]
          ]
          | setDocTryOption,
            dasheds <- maybeToList (NE.nonEmpty setDocDasheds)
        ],
        [ [ ["env var:"],
            envVarChunksNE vars
              ++ [" ", metavarChunk $ fromMaybe "ARG" setDocMetavar]
          ]
          | vars <- maybeToList setDocEnvVars
        ]
      ]
  where
    addHelpText :: [[[Chunk]]] -> [[[Chunk]]]
    addHelpText = \case
      [] -> [addHelpTextToLine []]
      [cs] -> [addHelpTextToLine cs]
      (l : ls) -> addHelpTextToLine l : ls

    addHelpTextToLine :: [[Chunk]] -> [[Chunk]]
    addHelpTextToLine = (++ [[helpChunk setDocHelp]])

renderManPage :: String -> AnyDocs SetDoc -> [Chunk]
renderManPage progname docs =
  let optDocs = docsToOptDocs docs
   in unlinesChunks
        [ renderShortOptDocs progname optDocs,
          [],
          [fore cyan "All settings:"],
          renderAnyDocs docs,
          [fore cyan "Options:"],
          renderLongOptDocs optDocs,
          [fore cyan "Environment Variables:"],
          renderEnvDocs (docsToEnvDocs docs),
          [fore cyan "Configuration Values:"],
          renderConfDocs (docsToConfDocs docs)
        ]

renderHelpPage :: String -> AnyDocs SetDoc -> [Chunk]
renderHelpPage progname docs =
  unlinesChunks
    [ renderShortOptDocs progname (docsToOptDocs docs),
      [],
      renderAnyDocs docs
    ]

renderAnyDocs :: AnyDocs SetDoc -> [Chunk]
renderAnyDocs = layoutAsTable . go
  where
    go :: AnyDocs SetDoc -> [[[Chunk]]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle d -> map (["  "] :) (renderSetDoc d)

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
      optDocMetavar = setDocMetavar
      optDocHelp = setDocHelp
  pure OptDoc {..}

renderShortOptDocs :: String -> AnyDocs OptDoc -> [Chunk]
renderShortOptDocs progname = unwordsChunks . (\cs -> [[fore yellow (chunk (T.pack progname))], cs]) . go
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
      AnyDocsSingle vs -> [["  "] : renderOptDocLong vs]

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
    [helpChunk optDocHelp]
  ]

parserEnvDocs :: Parser a -> AnyDocs EnvDoc
parserEnvDocs = docsToEnvDocs . parserDocs

docsToEnvDocs :: AnyDocs SetDoc -> AnyDocs EnvDoc
docsToEnvDocs = mapMaybeDocs setDocEnvDoc

setDocEnvDoc :: SetDoc -> Maybe EnvDoc
setDocEnvDoc SetDoc {..} = do
  envDocVars <- setDocEnvVars
  let envDocMetavar = setDocMetavar
  let envDocHelp = setDocHelp
  pure EnvDoc {..}

settingEnvDoc :: Setting a -> Maybe EnvDoc
settingEnvDoc = setDocEnvDoc . settingSetDoc

renderEnvDocs :: AnyDocs EnvDoc -> [Chunk]
renderEnvDocs = layoutAsTable . go
  where
    go :: AnyDocs EnvDoc -> [[[Chunk]]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> [["  "] : renderEnvDoc ed]

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
  let confDocHelp = setDocHelp
  pure ConfDoc {..}

settingConfDoc :: Setting a -> Maybe ConfDoc
settingConfDoc = setDocConfDoc . settingSetDoc

renderConfDocs :: AnyDocs ConfDoc -> [Chunk]
renderConfDocs = unlinesChunks . go
  where
    go :: AnyDocs ConfDoc -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> map ("  " :) (renderConfDoc ed)

renderConfDoc :: ConfDoc -> [[Chunk]]
renderConfDoc ConfDoc {..} =
  [helpChunk confDocHelp]
    : concatMap
      ( \(key, schema) ->
          [confValChunk key, ":"]
            : map ("  " :) (jsonSchemaChunkLines schema)
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

confValChunks :: Maybe (NonEmpty String) -> Maybe [Chunk]
confValChunks = fmap confValChunksNE

confValChunksNE :: NonEmpty String -> [Chunk]
confValChunksNE = intersperse (fore cyan "|") . map confValChunk . NE.toList

confValChunk :: String -> Chunk
confValChunk = fore white . chunk . T.pack

helpChunk :: Maybe Help -> Chunk
helpChunk = maybe (fore red "!! undocumented !!") (fore blue . chunk . T.pack)
