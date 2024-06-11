{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.Reader where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.String
import Text.Read

type Reader a = String -> Either String a

str :: (IsString s) => Reader s
str = Right . fromString

auto :: (Read a) => Reader a
auto s = case readMaybe s of
  Nothing -> Left $ "Un-Read-able value: " <> show s
  Just a -> a

-- | Always return True
exists :: Reader Bool
exists = const $ Right True

maybeReader :: (String -> Maybe a) -> Reader a
maybeReader func s = case func s of
  Nothing -> Left $ "Unparsable value: " <> show s
  Just a -> Right a

eitherReader :: (String -> Either String a) -> Reader a
eitherReader = id

commaSeparated :: Reader a -> Reader (NonEmpty a)
commaSeparated func = mapM func . parseCommaSeparated

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
