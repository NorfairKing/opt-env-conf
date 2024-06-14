{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.Test
  ( settingsLintTest,
    parserLintTest,
    pureGoldenManPage,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import OptEnvConf
import OptEnvConf.Lint
import Test.Syd
import Text.Colour

settingsLintTest :: forall a. (HasParser a) => Spec
settingsLintTest = do
  specify "pass the lint test" $
    parserLintTest (settingsParser @a)

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
