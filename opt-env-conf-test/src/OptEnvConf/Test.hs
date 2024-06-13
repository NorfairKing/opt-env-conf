{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.Test (parserLintTest) where

import qualified Data.Text as T
import OptEnvConf
import OptEnvConf.Lint
import Test.Syd
import Text.Colour

parserLintTest :: Parser a -> IO ()
parserLintTest parser =
  case lintParser parser of
    Nothing -> pure ()
    Just errs ->
      expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderLintErrors errs
