{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Reader where

import Control.Monad.Reader (MonadReader (..))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Text.Read

newtype Reader a = Reader {unReader :: String -> Either String a}
  deriving (Functor)

instance Applicative Reader where
  pure = Reader . const . Right
  (<*>) (Reader ff) (Reader fa) =
    Reader $ \s ->
      ff s <*> fa s

instance Monad Reader where
  (>>=) (Reader fa) fb = Reader $ \s -> do
    a <- fa s
    unReader (fb a) s

instance MonadReader String Reader where
  ask = Reader Right
  reader f = Reader $ \s -> Right (f s)
  local fs f = Reader $ \s ->
    let s' = fs s
     in unReader f s'

runReader :: Reader a -> String -> Either String a
runReader = unReader

str :: (IsString s) => Reader s
str = Reader $ Right . fromString

-- | Read via the 'Read' instance
--
-- You cannot use this for bare strings, because 'Read' for strings parses quotes.
auto :: (Read a) => Reader a
auto = Reader $ \s -> case readMaybe s of
  Nothing -> Left $ "Un-Read-able value: " <> show s
  Just a -> Right a

-- | Always return True
exists :: Reader Bool
exists = maybeReader $ const $ Just True

maybeReader :: (String -> Maybe a) -> Reader a
maybeReader func = Reader $ \s -> case func s of
  Nothing -> Left $ "Unparsable value: " <> show s
  Just a -> Right a

eitherReader :: (String -> Either String a) -> Reader a
eitherReader = Reader

commaSeparatedSet :: (Ord a) => Reader a -> Reader (Set a)
commaSeparatedSet func = S.fromList <$> commaSeparatedList func

commaSeparatedList :: Reader a -> Reader [a]
commaSeparatedList func = NE.toList <$> commaSeparated func

commaSeparated :: Reader a -> Reader (NonEmpty a)
commaSeparated (Reader func) = Reader $ mapM func . parseCommaSeparated

renderCommaSeparated :: NonEmpty String -> String
renderCommaSeparated = intercalate "," . map escape . NE.toList
  where
    escape = concatMap $ \case
      ',' -> "\\,"
      '\\' -> "\\\\"
      c -> [c]

parseCommaSeparated :: String -> NonEmpty String
parseCommaSeparated = go ""
  where
    go acc = \case
      [] -> reverse acc :| []
      '\\' : '\\' : rest -> go ('\\' : acc) rest
      '\\' : ',' : rest -> go (',' : acc) rest
      ',' : rest -> reverse acc <| go "" rest
      c : rest -> go (c : acc) rest
