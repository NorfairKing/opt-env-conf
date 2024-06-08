{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf.Doc where

import Data.List (intercalate, intersperse)
import Data.Maybe
import Data.Text (Text)
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
  { optDocDasheds :: ![Dashed],
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
      ParserArg _ o -> AnyDocsSingle $ AnyDocOpt $ argumentOptDoc o
      ParserOpt _ o -> AnyDocsSingle $ AnyDocOpt $ optionOptDoc o
      ParserSwitch _ o -> AnyDocsSingle $ AnyDocOpt $ switchOptDoc o
      ParserEnvVar _ o -> AnyDocsSingle $ AnyDocEnv $ envEnvDoc o
      ParserPrefixed prefix p -> anyDocPrefixed prefix <$> go p
      ParserConfig _ _ -> noDocs

anyDocPrefixed :: String -> AnyDoc -> AnyDoc
anyDocPrefixed prefix = \case
  AnyDocEnv ed -> AnyDocEnv $ ed {envDocVars = map (prefix <>) (envDocVars ed)}
  ad -> ad

argumentOptDoc :: ArgumentParser a -> OptDoc
argumentOptDoc OptionGenerals {..} =
  OptDoc
    { optDocDasheds = [],
      optDocMetavar = argumentSpecificsMetavar optionGeneralSpecifics,
      optDocHelp = optionGeneralHelp
    }

optionOptDoc :: OptionParser a -> OptDoc
optionOptDoc OptionGenerals {..} =
  OptDoc
    { optDocDasheds = optionSpecificsDasheds optionGeneralSpecifics,
      optDocMetavar = optionSpecificsMetavar optionGeneralSpecifics,
      optDocHelp = optionGeneralHelp
    }

switchOptDoc :: SwitchParser a -> OptDoc
switchOptDoc OptionGenerals {..} =
  OptDoc
    { optDocDasheds = switchSpecificsDasheds optionGeneralSpecifics,
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
  AnyDocOpt d -> renderOptDocLong d
  AnyDocEnv d -> renderEnvDoc d

renderManPage :: AnyDocs AnyDoc -> [Chunk]
renderManPage = renderHelpPage

renderHelpPage :: AnyDocs AnyDoc -> [Chunk]
renderHelpPage =
  unlinesChunks
    . go
  where
    go :: AnyDocs AnyDoc -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> renderAnyDoc vs

parserOptDocs :: Parser a -> OptDocs
parserOptDocs =
  fromMaybe (AnyDocsAnd [])
    . traverse
      ( \case
          AnyDocOpt d -> Just d
          _ -> Nothing
      )
    . parserDocs

renderCompleteOptDocs :: OptDocs -> [Chunk]
renderCompleteOptDocs optDocs =
  unlinesChunks
    [ renderShortOptDocs optDocs,
      renderLongOptDocs optDocs
    ]

renderShortOptDocs :: OptDocs -> [Chunk]
renderShortOptDocs = go
  where
    go :: OptDocs -> [Chunk]
    go = \case
      AnyDocsAnd ds -> unwordsChunks $ map go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle v ->
        ( \OptDoc {..} ->
            concat
              [ [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed optDocDasheds
                  | not (null optDocDasheds)
                ],
                [ chunk $ T.pack $ fromMaybe "ARG" optDocMetavar
                ]
              ]
        )
          v

renderLongOptDocs :: OptDocs -> [Chunk]
renderLongOptDocs =
  layoutAsTable
    . go
  where
    go :: OptDocs -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> renderOptDocLong vs

renderOptDocLong :: OptDoc -> [[Chunk]]
renderOptDocLong OptDoc {..} =
  [ intersperse
      " "
      $ concat
        [ [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed optDocDasheds
            | not (null optDocDasheds)
          ],
          [ chunk . T.pack $ fromMaybe "[ARG]" optDocMetavar,
            chunk . T.pack $ fromMaybe "undocumented" optDocHelp
          ]
        ]
  ]

parserEnvDocs :: Parser a -> EnvDocs
parserEnvDocs =
  fromMaybe (AnyDocsAnd [])
    . traverse
      ( \case
          AnyDocEnv d -> Just d
          _ -> Nothing
      )
    . parserDocs

renderEnvDocs :: EnvDocs -> Text
renderEnvDocs =
  renderChunksText With24BitColours
    . layoutAsTable
    . go
  where
    go :: EnvDocs -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> renderEnvDoc vs

renderEnvDoc :: EnvDoc -> [[Chunk]]
renderEnvDoc EnvDoc {..} =
  [ intersperse " " $
      concat
        [ [ chunk . T.pack $ intercalate "|" envDocVars
            | not (null envDocVars)
          ],
          [ chunk . T.pack $ fromMaybe "undocumented" envDocHelp
          ]
        ]
  ]

unwordsChunks :: [[Chunk]] -> [Chunk]
unwordsChunks = intercalate [" "]
