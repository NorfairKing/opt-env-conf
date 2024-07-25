{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Reader
  ( Reader (..),

    -- * Common readers
    str,
    auto,
    exists,
    viaCodec,

    -- * Constructing your own reader
    maybeReader,
    eitherReader,

    -- * Comma-separated readers
    commaSeparated,
    commaSeparatedList,
    commaSeparatedSet,

    -- * Internal
    runReader,
    renderCommaSeparated,
    parseCommaSeparated,
  )
where

import Autodocodec
import Control.Monad.Reader (MonadReader (..))
import Data.Aeson.Types as JSON
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import Text.Read (readMaybe)

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

-- | Read a string as-is.
--
-- __This is the reader you will want to use for reading a 'String'.__
--
-- This is different from 'auto' for strings because 'Read' wants to parse quotes when parsing Strings.
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
--
-- > exists = Reader $ const $ pure True
exists :: Reader Bool
exists = Reader $ const $ pure True

viaCodec :: (HasCodec a) => Reader a
viaCodec = eitherReader $ parseEither $ parseJSONViaCodec . JSON.String . T.pack

-- | Turn a 'Maybe' parsing function into a 'Reader'
maybeReader :: (String -> Maybe a) -> Reader a
maybeReader func = Reader $ \s -> case func s of
  Nothing -> Left $ "Unparsable value: " <> show s
  Just a -> Right a

-- | Turn an 'Either' parsing function into a 'Reader'
--
-- API note: This is a forward-compatible alias for 'Reader'.
eitherReader :: (String -> Either String a) -> Reader a
eitherReader = Reader

-- | Like 'commaSeparated' but uses a set type.
--
-- Note that this will never parse the empty list, so prefer 'commaSeparated'
-- if you want a more accurately typed function.
--
-- Note also that this function throws away any ordering information and
-- ignores any duplicate values.
commaSeparatedSet :: (Ord a) => Reader a -> Reader (Set a)
commaSeparatedSet func = S.fromList <$> commaSeparatedList func

-- | Like 'commaSeparated' but uses a list type.
--
-- Note that this will never parse the empty list, so prefer 'commaSeparated'
-- if you want a more accurately typed function.
commaSeparatedList :: Reader a -> Reader [a]
commaSeparatedList func = NE.toList <$> commaSeparated func

-- | Turn a reader into one that parses comma separated values with that reader.
commaSeparated :: Reader a -> Reader (NonEmpty a)
commaSeparated (Reader func) = Reader $ mapM func . parseCommaSeparated

-- | Separate by commas and escape commas in values
renderCommaSeparated :: NonEmpty String -> String
renderCommaSeparated = intercalate "," . map escape . NE.toList
  where
    escape = concatMap $ \case
      ',' -> "\\,"
      '\\' -> "\\\\"
      c -> [c]

-- | Parse comma separated string, ignore escaped commas
parseCommaSeparated :: String -> NonEmpty String
parseCommaSeparated = go ""
  where
    go acc = \case
      [] -> reverse acc :| []
      '\\' : '\\' : rest -> go ('\\' : acc) rest
      '\\' : ',' : rest -> go (',' : acc) rest
      ',' : rest -> reverse acc <| go "" rest
      c : rest -> go (c : acc) rest
