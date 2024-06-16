{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.Test
  ( settingsLintSpec,
    parserLintTest,
    goldenReferenceDocumentationSpec,
    pureGoldenReferenceDocumentation,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import OptEnvConf
import OptEnvConf.Lint
import Test.Syd
import Text.Colour

settingsLintSpec :: forall a. (HasParser a) => Spec
settingsLintSpec = do
  specify "pass the lint test" $
    parserLintTest (settingsParser @a)

parserLintTest :: Parser a -> IO ()
parserLintTest parser =
  case lintParser parser of
    Nothing -> pure ()
    Just errs ->
      expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderLintErrors errs

goldenReferenceDocumentationSpec :: forall a. (HasParser a) => FilePath -> String -> Spec
goldenReferenceDocumentationSpec path progname = do
  specify "produces the same reference documentation as before" $
    pureGoldenReferenceDocumentation path progname (settingsParser @a)

pureGoldenReferenceDocumentation :: FilePath -> String -> Parser a -> GoldenTest Text
pureGoldenReferenceDocumentation path progname parser =
  pureGoldenTextFile path $
    renderChunksText With24BitColours $
      renderReferenceDocumentation progname $
        parserDocs parser
