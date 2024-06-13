{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.Test
  ( parserLintTest,
    pureGoldenManPage,
  )
where

import Data.Text (Text)
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

pureGoldenManPage :: FilePath -> String -> Parser a -> GoldenTest Text
pureGoldenManPage path progname parser =
  pureGoldenTextFile path $
    renderChunksText With24BitColours $
      renderManPage progname $
        parserDocs parser
