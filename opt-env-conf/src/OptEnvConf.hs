{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptEnvConf
  ( module OptEnvConf,
    module OptEnvConf.Run,
    Parser,
    HasParser (..),
    module Control.Applicative,
  )
where

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import OptEnvConf.ArgMap (Dashed (..))
import qualified OptEnvConf.ArgMap as AM
import OptEnvConf.Parser
import OptEnvConf.Run
import Text.Colour
import Text.Colour.Layout

data ArgParser a = ArgParser
  { argParserParse :: !(String -> Either String a),
    argParserShort :: ![Char], -- TODO use dashed?
    argParserLong :: ![String]
  }

data EnvParser a = EnvParser
  { envParserParse :: !(String -> Either String a),
    envParserVar :: !String
  }

envVar :: String -> Parser (Maybe String)
envVar = ParserEnvVar

strArg :: Parser String
strArg = ParserArg

strArgs :: Parser [String]
strArgs = ParserArgs

strOpt :: String -> Parser (Maybe String)
strOpt = ParserOpt . NE.singleton . DashedLong . NE.fromList -- TODO unsafe

argLeftovers :: Parser [String]
argLeftovers = ParserArgLeftovers

-- Arguments _and_ leftovers
allArgs :: Parser [String]
allArgs = (++) <$> strArgs <*> argLeftovers

confVar :: String -> Parser (Maybe String)
confVar = ParserConfig

optionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
optionalFirst = ParserOptionalFirst

requiredFirst :: [Parser (Maybe a)] -> Parser a
requiredFirst = ParserRequiredFirst

parserDocs :: Parser a -> AnyDocs AnyDoc
parserDocs = go
  where
    go :: Parser a -> AnyDocs AnyDoc
    go = \case
      ParserFmap _ p -> go p
      ParserPure _ -> AnyDocsSingle []
      ParserAp pf pa -> AnyDocsAnd [go pf, go pa]
      ParserAlt p1 p2 -> AnyDocsOr [go p1, go p2]
      ParserOptionalFirst ps -> AnyDocsOr $ map go ps
      ParserRequiredFirst ps -> AnyDocsOr $ map go ps
      ParserArg -> AnyDocsSingle [AnyDocOpt $ OptDocArg Nothing]
      ParserArgs -> AnyDocsSingle [AnyDocOpt $ OptDocArgs Nothing]
      ParserOpt d -> AnyDocsSingle [AnyDocOpt $ OptDocOpt d Nothing]
      ParserArgLeftovers -> AnyDocsSingle [AnyDocOpt $ OptDocLeftovers Nothing]
      ParserEnvVar v ->
        AnyDocsSingle
          [ AnyDocEnv $
              EnvDoc
                { envDocVar = v,
                  envDocHelp = Nothing
                }
          ]
      ParserConfig _ -> AnyDocsSingle []

renderDocs :: AnyDocs AnyDoc -> [Chunk]
renderDocs =
  unlinesChunks
    . go
    . simplifyAnyDocs
  where
    go :: AnyDocs AnyDoc -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> concatMap renderAnyDoc vs

data AnyDoc
  = AnyDocOpt !OptDoc
  | AnyDocEnv !EnvDoc

renderAnyDoc :: AnyDoc -> [[Chunk]]
renderAnyDoc = \case
  AnyDocOpt d -> renderOptDocLong d
  AnyDocEnv d -> renderEnvDoc d

data AnyDocs a
  = AnyDocsAnd ![AnyDocs a]
  | AnyDocsOr ![AnyDocs a]
  | AnyDocsSingle ![a]

instance Functor AnyDocs where
  fmap f = \case
    AnyDocsAnd as -> AnyDocsAnd $ fmap (fmap f) as
    AnyDocsOr as -> AnyDocsOr $ fmap (fmap f) as
    AnyDocsSingle as -> AnyDocsSingle $ fmap f as

instance Foldable AnyDocs where
  foldMap f = \case
    AnyDocsAnd as -> foldMap (foldMap f) as
    AnyDocsOr as -> foldMap (foldMap f) as
    AnyDocsSingle as -> foldMap f as

instance Traversable AnyDocs where
  traverse f = \case
    AnyDocsAnd as -> AnyDocsAnd <$> traverse (traverse f) as
    AnyDocsOr as -> AnyDocsOr <$> traverse (traverse f) as
    AnyDocsSingle as -> AnyDocsSingle <$> traverse f as

type OptDocs = AnyDocs OptDoc

data OptDoc
  = OptDocArg !(Maybe String)
  | OptDocArgs !(Maybe String)
  | OptDocOpt !(NonEmpty Dashed) !(Maybe String)
  | OptDocLeftovers !(Maybe String)

parserOptDocs :: Parser a -> OptDocs
parserOptDocs =
  fromMaybe (AnyDocsSingle [])
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
renderShortOptDocs = go . simplifyAnyDocs
  where
    go :: OptDocs -> [Chunk]
    go = \case
      AnyDocsAnd ds -> unwordsChunks $ map go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs ->
        unwordsChunks $
          map
            ( \case
                OptDocArg _ ->
                  [ "ARG"
                  ]
                OptDocArgs _ ->
                  [ "[ARG]"
                  ]
                OptDocOpt flags _ ->
                  [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed $ NE.toList flags
                  ]
                OptDocLeftovers _ ->
                  [ "LEFTOVERS"
                  ]
            )
            vs

unwordsChunks :: [[Chunk]] -> [Chunk]
unwordsChunks = intercalate [" "]

renderLongOptDocs :: OptDocs -> [Chunk]
renderLongOptDocs =
  layoutAsTable
    . go
    . simplifyAnyDocs
  where
    go :: OptDocs -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> concatMap renderOptDocLong vs

renderOptDocLong :: OptDoc -> [[Chunk]]
renderOptDocLong =
  (: []) . \case
    OptDocArg mDoc ->
      [ "ARG",
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]
    OptDocArgs mDoc ->
      [ "[ARG]",
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]
    OptDocOpt flags mDoc ->
      [ chunk . T.pack $ intercalate "|" $ map AM.renderDashed $ NE.toList flags,
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]
    OptDocLeftovers mDoc ->
      [ "leftovers",
        chunk . T.pack $ fromMaybe "undocumented" mDoc
      ]

type EnvDocs = AnyDocs EnvDoc

data EnvDoc = EnvDoc
  { envDocVar :: !String,
    envDocHelp :: !(Maybe String)
  }

parserEnvDocs :: Parser a -> EnvDocs
parserEnvDocs =
  fromMaybe (AnyDocsSingle [])
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
    . simplifyAnyDocs
  where
    go :: EnvDocs -> [[Chunk]]
    go = \case
      AnyDocsAnd ds -> concatMap go ds
      AnyDocsOr ds -> concatMap go ds
      AnyDocsSingle vs -> concatMap renderEnvDoc vs

renderEnvDoc :: EnvDoc -> [[Chunk]]
renderEnvDoc EnvDoc {..} =
  [ [ chunk $ T.pack envDocVar,
      chunk . T.pack $ fromMaybe "undocumented" envDocHelp
    ]
  ]

simplifyAnyDocs :: AnyDocs a -> AnyDocs a
simplifyAnyDocs = go
  where
    go = \case
      AnyDocsAnd ds -> AnyDocsAnd $ concatMap goAnd ds
      AnyDocsOr ds -> AnyDocsOr $ concatMap goOr ds
      AnyDocsSingle vs -> AnyDocsSingle vs

    goAnd = \case
      AnyDocsAnd ds -> concatMap goAnd ds
      ds -> [ds]

    goOr = \case
      AnyDocsOr ds -> concatMap goOr ds
      ds -> [ds]
