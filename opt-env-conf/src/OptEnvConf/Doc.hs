{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Doc where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import OptEnvConf.ArgMap (Dashed (..))
import qualified OptEnvConf.ArgMap as AM
import OptEnvConf.Opt
import OptEnvConf.Parser
import Text.Colour
import Text.Colour.Layout

data AnyDoc
  = AnyDocOpt !OptDoc
  | AnyDocEnv !EnvDoc
  deriving (Show)

data OptDoc = OptDoc
  { optDocNeedsArg :: !Bool,
    optDocDasheds :: ![Dashed],
    optDocMetavar :: !(Maybe Metavar),
    optDocHelp :: !(Maybe String)
  }
  deriving (Show, Eq)

data EnvDoc = EnvDoc
  { envDocVars :: ![String],
    envDocMetavar :: !(Maybe Metavar),
    envDocHelp :: !(Maybe String)
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

type OptDocs = AnyDocs OptDoc

type EnvDocs = AnyDocs EnvDoc

parserDocs :: Parser a -> AnyDocs AnyDoc
parserDocs = simplifyAnyDocs . go
  where
    noDocs = AnyDocsAnd []
    go :: Parser a -> AnyDocs AnyDoc
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
      ParserPrefixed prefix p -> anyDocPrefixed prefix <$> go p
      ParserSetting p ->
        AnyDocsOr $
          catMaybes
            [ AnyDocsSingle . AnyDocOpt <$> settingOptDoc p,
              AnyDocsSingle . AnyDocEnv <$> settingEnvDoc p
            ] -- TODO

settingOptDoc :: SettingParser a -> Maybe OptDoc
settingOptDoc OptionGenerals {..} = do
  pure $
    OptDoc
      { optDocNeedsArg = True,
        optDocDasheds = settingSpecificsDasheds optionGeneralSpecifics,
        optDocMetavar = settingSpecificsMetavar optionGeneralSpecifics,
        optDocHelp = optionGeneralHelp
      }

settingEnvDoc :: SettingParser a -> Maybe EnvDoc
settingEnvDoc OptionGenerals {..} = do
  vars <- NE.nonEmpty $ settingSpecificsEnvVars optionGeneralSpecifics
  pure $
    EnvDoc
      { envDocVars = NE.toList vars,
        envDocMetavar = settingSpecificsMetavar optionGeneralSpecifics,
        envDocHelp = optionGeneralHelp
      }

anyDocPrefixed :: String -> AnyDoc -> AnyDoc
anyDocPrefixed prefix = \case
  AnyDocEnv ed -> AnyDocEnv $ ed {envDocVars = map (prefix <>) (envDocVars ed)}
  ad -> ad

argumentOptDoc :: ArgumentParser a -> OptDoc
argumentOptDoc OptionGenerals {..} =
  OptDoc
    { optDocNeedsArg = True,
      optDocDasheds = [],
      optDocMetavar = argumentSpecificsMetavar optionGeneralSpecifics,
      optDocHelp = optionGeneralHelp
    }

optionOptDoc :: OptionParser a -> OptDoc
optionOptDoc OptionGenerals {..} =
  OptDoc
    { optDocNeedsArg = True,
      optDocDasheds = optionSpecificsDasheds optionGeneralSpecifics,
      optDocMetavar = optionSpecificsMetavar optionGeneralSpecifics,
      optDocHelp = optionGeneralHelp
    }

switchOptDoc :: SwitchParser a -> OptDoc
switchOptDoc OptionGenerals {..} =
  OptDoc
    { optDocNeedsArg = False,
      optDocDasheds = switchSpecificsDasheds optionGeneralSpecifics,
      optDocMetavar = Nothing,
      optDocHelp = optionGeneralHelp
    }

envEnvDoc :: EnvParser a -> EnvDoc
envEnvDoc OptionGenerals {..} =
  EnvDoc
    { envDocVars = envSpecificsVars optionGeneralSpecifics,
      envDocMetavar = envSpecificsMetavar optionGeneralSpecifics,
      envDocHelp = optionGeneralHelp
    }

renderAnyDoc :: AnyDoc -> [[Chunk]]
renderAnyDoc = \case
  AnyDocOpt d -> ["Option: "] : renderOptDocLong d
  AnyDocEnv d -> ["Env var:"] : renderEnvDoc d

renderManPage :: String -> AnyDocs AnyDoc -> [Chunk]
renderManPage progname docs =
  let optDocs = docsToOptDocs docs
   in unlinesChunks
        [ renderShortOptDocs progname optDocs,
          [],
          ["All settings:"],
          renderAnyDocs docs,
          ["Options:"],
          renderLongOptDocs optDocs,
          ["Environment Variables:"],
          renderEnvDocs (docsToEnvDocs docs)
        ]

renderHelpPage :: String -> AnyDocs AnyDoc -> [Chunk]
renderHelpPage progname docs =
  unlinesChunks
    [ renderShortOptDocs progname (docsToOptDocs docs),
      [],
      renderAnyDocs docs
    ]

renderAnyDocs :: AnyDocs AnyDoc -> [Chunk]
renderAnyDocs = layoutAsTable . go
  where
    go :: AnyDocs AnyDoc -> [[[Chunk]]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle d -> [["  "] : renderAnyDoc d]

parserOptDocs :: Parser a -> AnyDocs OptDoc
parserOptDocs = docsToOptDocs . parserDocs

docsToOptDocs :: AnyDocs AnyDoc -> OptDocs
docsToOptDocs = mapMaybeDocs go
  where
    go = \case
      AnyDocOpt o -> Just o
      _ -> Nothing

renderShortOptDocs :: String -> OptDocs -> [Chunk]
renderShortOptDocs progname = unwordsChunks . (\cs -> [[fore yellow (chunk (T.pack progname))], cs]) . go
  where
    go :: OptDocs -> [Chunk]
    go = \case
      AnyDocsAnd ds -> unwordsChunks $ map go ds
      AnyDocsOr ds -> renderOrChunks (map go ds)
      AnyDocsSingle OptDoc {..} ->
        unwordsChunks $
          concat
            [ maybeToList $ dashedChunks optDocDasheds,
              [ [metavarChunk $ fromMaybe "ARG" optDocMetavar]
                | optDocNeedsArg
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

renderLongOptDocs :: OptDocs -> [Chunk]
renderLongOptDocs = layoutAsTable . go
  where
    go :: OptDocs -> [[[Chunk]]]
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
            | optDocNeedsArg
          ]
        ],
    [helpChunk optDocHelp]
  ]

parserEnvDocs :: Parser a -> EnvDocs
parserEnvDocs = docsToEnvDocs . parserDocs

docsToEnvDocs :: AnyDocs AnyDoc -> EnvDocs
docsToEnvDocs = mapMaybeDocs go
  where
    go = \case
      AnyDocEnv o -> Just o
      _ -> Nothing

renderEnvDocs :: EnvDocs -> [Chunk]
renderEnvDocs = layoutAsTable . go
  where
    go :: EnvDocs -> [[[Chunk]]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle ed -> [["  "] : renderEnvDoc ed]

renderEnvDoc :: EnvDoc -> [[Chunk]]
renderEnvDoc EnvDoc {..} =
  [ unwordsChunks $
      concat
        [ maybeToList $ envVarChunks envDocVars,
          [ [ metavarChunk $ fromMaybe "ARG" envDocMetavar
            ]
          ]
        ],
    [helpChunk envDocHelp]
  ]

metavarChunk :: Metavar -> Chunk
metavarChunk = fore yellow . chunk . T.pack

dashedChunks :: [Dashed] -> Maybe [Chunk]
dashedChunks = fmap dashedChunksNE . NE.nonEmpty

dashedChunksNE :: NonEmpty Dashed -> [Chunk]
dashedChunksNE = intersperse (fore cyan "|") . map dashedChunk . NE.toList

dashedChunk :: Dashed -> Chunk
dashedChunk = fore white . chunk . T.pack . AM.renderDashed

envVarChunks :: [String] -> Maybe [Chunk]
envVarChunks = fmap envVarChunksNE . NE.nonEmpty

envVarChunksNE :: NonEmpty String -> [Chunk]
envVarChunksNE = intersperse (fore cyan "|") . map envVarChunk . NE.toList

envVarChunk :: String -> Chunk
envVarChunk = fore white . chunk . T.pack

helpChunk :: Maybe Help -> Chunk
helpChunk = maybe (fore red "!! undocumented !!") (fore blue . chunk . T.pack)
