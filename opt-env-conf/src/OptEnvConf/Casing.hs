{-# LANGUAGE LambdaCase #-}

module OptEnvConf.Casing
  ( toArgCase,
    toEnvCase,
    toConfigCase,
  )
where

import qualified Data.Char as Char

-- this-is-arg-case
toArgCase :: String -> String
toArgCase = toConfigCase

-- THIS_IS_ENV_CASE
toEnvCase :: String -> String
toEnvCase = map (Char.toUpper . spacer '_')

-- this-is-config-case
toConfigCase :: String -> String
toConfigCase = map (Char.toLower . spacer '-')

spacer :: Char -> Char -> Char
spacer s = \case
  '-' -> s
  '_' -> s
  ' ' -> s
  c -> c
