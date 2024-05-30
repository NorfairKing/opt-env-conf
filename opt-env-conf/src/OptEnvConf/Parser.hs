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

import Control.Applicative
import Data.Aeson (FromJSON)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import OptEnvConf.Opt
import OptEnvConf.Reader
import Text.Show

data Parser a where
  -- Functor
  ParserPure :: a -> Parser a
  -- Applicative
  ParserFmap :: (a -> b) -> Parser a -> Parser b
  ParserAp :: Parser (a -> b) -> Parser a -> Parser b
  -- Alternative
  ParserEmpty :: Parser a
  ParserAlt :: Parser a -> Parser a -> Parser a
  ParserMany :: Parser a -> Parser [a]
  ParserSome :: Parser a -> Parser (NonEmpty a)
  -- IO
  --

  -- | Apply a computation to the result of a parser
  --
  -- This is intended for use-cases like resolving a file to an absolute path.
  -- It is morally ok for read-only IO actions but you will
  -- have a bad time if the action is not read-only.
  ParserMapIO :: (a -> IO b) -> Parser a -> Parser b
  -- Combining
  -- TODO Maybe we can get rid of this constructor using 'optional requiredFirst'
  ParserOptionalFirst :: [Parser (Maybe a)] -> Parser (Maybe a)
  -- TODO maybe we can get rid of this constructor using Alt
  ParserRequiredFirst :: [Parser (Maybe a)] -> Parser a
  -- | Arguments and options
  ParserArg :: !(Reader a) -> !(ArgumentParser a) -> Parser a
  -- TODO consider getting rid of ParserOpt and "just" giving Arg a possibly-empty list of dasheds to parse
  ParserOpt :: !(Reader a) -> !(OptionParser a) -> Parser a
  -- | Env vars
  ParserEnvVar :: String -> Parser (Maybe String)
  -- | Configuration file
  ParserConfig :: FromJSON a => String -> Parser (Maybe a)

instance Functor Parser where
  fmap = ParserFmap

instance Applicative Parser where
  pure = ParserPure
  (<*>) = ParserAp

instance Alternative Parser where
  empty = ParserEmpty
  (<|>) p1 p2 =
    let isEmpty :: Parser a -> Bool
        isEmpty = \case
          ParserEmpty -> True
          ParserFmap _ p' -> isEmpty p'
          ParserRequiredFirst [] -> True
          _ -> False
     in case (isEmpty p1, isEmpty p2) of
          (True, True) -> ParserEmpty
          (True, False) -> p2
          (False, True) -> p1
          (False, False) -> ParserAlt p1 p2
  many = ParserMany

  -- TODO maybe we can get rid of the some constructor by using (:) <$> p <$> many p
  some = fmap NE.toList . ParserSome

class HasParser a where
  optEnvConfParser :: Parser a

showParserABit :: Parser a -> String
showParserABit = ($ "") . go 0
  where
    go :: Int -> Parser a -> ShowS
    go d = \case
      ParserFmap _ p ->
        showParen (d > 10) $
          showString "Fmap _ "
            . go 11 p
      ParserPure _ -> showParen (d > 10) $ showString "Pure _"
      ParserAp pf pa ->
        showParen (d > 10) $
          showString "Ap "
            . go 11 pf
            . go 11 pa
      ParserEmpty -> showString "Empty"
      ParserAlt p1 p2 ->
        showParen (d > 10) $
          showString "Alt "
            . go 11 p1
            . showString " "
            . go 11 p2
      ParserMany p ->
        showParen (d > 10) $
          showString "Many "
            . go 11 p
      ParserSome p ->
        showParen (d > 10) $
          showString "Some "
            . go 11 p
      ParserMapIO _ p ->
        showParen (d > 10) $
          showString "MapIO _ "
            . go 11 p
      ParserOptionalFirst ps ->
        showParen (d > 10) $
          showString "OptionalFirst "
            . showListWith (go 11) ps
      ParserRequiredFirst ps ->
        showParen (d > 10) $
          showString "RequiredFirst "
            . showListWith (go 11) ps
      ParserArg _ p ->
        showParen (d > 10) $
          showString "Arg _ "
            . showArgumentParserABit p
      ParserOpt _ p ->
        showParen (d > 10) $
          showString "Opt _ "
            . showOptionParserABit p
      ParserEnvVar v ->
        showParen (d > 10) $
          showString "EnvVar " . showsPrec 11 v
      ParserConfig key ->
        showParen (d > 10) $
          showString "Config "
            . showsPrec 11 key
