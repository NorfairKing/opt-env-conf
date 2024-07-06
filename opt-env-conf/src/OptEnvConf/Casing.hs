{-# LANGUAGE LambdaCase #-}

module OptEnvConf.Casing
  ( -- * Casing
    toArgCase,
    toEnvCase,
    toConfigCase,

    -- * Internal
    toShellFunctionCase,
  )
where

import qualified Data.Char as Char

-- | Turn a string into arg case for option names
--
-- Example: @this-is-arg-case@
toArgCase :: String -> String
toArgCase = toConfigCase

-- | Turn a string into env case for environment variable names
--
-- Example: @THIS_IS_ENV_CASE@
toEnvCase :: String -> String
toEnvCase = map (Char.toUpper . spacer '_')

-- | Turn a string into config case for configuration value names
--
-- Example: @this-is-config-case@
toConfigCase :: String -> String
toConfigCase = map (Char.toLower . spacer '-')

-- | Turn a string into a string that can be used as a shell function name (for completion)
--
-- Example: @this_is_shell_function_case@
toShellFunctionCase :: String -> String
toShellFunctionCase = map (Char.toLower . spacer '_')

spacer :: Char -> Char -> Char
spacer s = \case
  '-' -> s
  '_' -> s
  '.' -> s
  ' ' -> s
  '\t' -> s
  c -> c
