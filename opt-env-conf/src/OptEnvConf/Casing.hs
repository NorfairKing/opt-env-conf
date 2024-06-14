{-# LANGUAGE LambdaCase #-}

module OptEnvConf.Casing
  ( toArgCase,
    toEnvCase,
    toConfigCase,
  )
where

import qualified Data.Char as Char

toArgCase :: String -> String
toArgCase = toConfigCase

toEnvCase :: String -> String
toEnvCase = map (Char.toUpper . spacer '_')

toConfigCase :: String -> String
toConfigCase = map (Char.toLower . spacer '-')

spacer :: Char -> Char -> Char
spacer s = \case
  '-' -> s
  '_' -> s
  ' ' -> s
  c -> c
