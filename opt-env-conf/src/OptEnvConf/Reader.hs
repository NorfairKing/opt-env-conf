module OptEnvConf.Reader where

import Data.String
import Text.Read

type Reader a = String -> Either String a

str :: IsString s => Reader s
str = Right . fromString

auto :: Read a => Reader a
auto s = case readMaybe s of
  Nothing -> Left $ "Un-Read-able value: " <> show s
  Just a -> a

maybeReader :: (String -> Maybe a) -> Reader a
maybeReader func s = case func s of
  Nothing -> Left $ "Unparsable value: " <> show s
  Just a -> Right a

eitherReader :: (String -> Either String a) -> Reader a
eitherReader = id
