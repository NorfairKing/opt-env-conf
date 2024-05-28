{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Parser
  ( Parser (..),
    Metavar,
    Help,
    HasParser (..),
    showParserABit,
  )
where

import Data.Aeson (FromJSON)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (Dashed (..))
import OptEnvConf.Opt
import Text.Show

data Parser a where
  -- Functor
  ParserPure :: a -> Parser a
  -- Applicative
  ParserFmap :: (a -> b) -> Parser a -> Parser b
  ParserAp :: Parser (a -> b) -> Parser a -> Parser b
  -- Alternative
  ParserAlt :: Parser a -> Parser a -> Parser a
  -- Combining
  ParserOptionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
  ParserRequiredFirst :: [Parser (Maybe a)] -> Parser a
  -- | Arguments and options
  ParserArg :: !(Maybe Metavar) -> Parser String
  ParserArgs :: !(Maybe Metavar) -> Parser [String]
  ParserOpt :: !(OptionParser a) -> Parser (Maybe a)
  -- | Env vars
  ParserEnvVar :: String -> Parser (Maybe String)
  -- | Configuration file
  ParserConfig :: FromJSON a => String -> Parser (Maybe a)

instance Functor Parser where
  fmap = ParserFmap

instance Applicative Parser where
  pure = ParserPure
  (<*>) = ParserAp

class HasParser a where
  optEnvParser :: Parser a

showParserABit :: Parser a -> String
showParserABit = ($ "") . go 0
  where
    go :: Int -> Parser a -> ShowS
    go d = \case
      ParserFmap _ p -> showParen (d > 10) $ showString "Fmap _ " . go 11 p
      ParserPure _ -> showParen (d > 10) $ showString "Pure _"
      ParserAp pf pa -> showParen (d > 10) $ showString "Ap " . go 11 pf . go 11 pa
      ParserAlt p1 p2 -> showParen (d > 10) $ showString "Alt " . go 11 p1 . showString " " . go 11 p2
      ParserOptionalFirst ps -> showParen (d > 10) $ showString "OptionalFirst " . showListWith (go 11) ps
      ParserRequiredFirst ps -> showParen (d > 10) $ showString "RequiredFirst " . showListWith (go 11) ps
      ParserArg metavar ->
        showString "Arg "
          . showsPrec 10 metavar
      ParserArgs metavar ->
        showString "Args "
          . showsPrec 10 metavar
      ParserOpt p ->
        showParen (d > 10) $
          showString "Opt "
            . showOptionParserABit p
      ParserEnvVar v ->
        showParen (d > 10) $
          showString "EnvVar " . showsPrec 11 v
      ParserConfig key -> showParen (d > 10) $ showString "Config " . showsPrec 11 key
