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
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.Lint
import Test.Syd
import Text.Colour

settingsLintSpec :: forall a. (HasCallStack) => (HasParser a) => Spec
settingsLintSpec = withFrozenCallStack $ do
  specify "pass the lint test" $
    parserLintTest (settingsParser @a)

parserLintTest :: Parser a -> IO ()
parserLintTest parser =
  case lintParser parser of
    Nothing -> pure ()
    Just errs ->
      expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderLintErrors errs

goldenReferenceDocumentationSpec :: forall a. (HasCallStack) => (HasParser a) => FilePath -> String -> Spec
goldenReferenceDocumentationSpec path progname = withFrozenCallStack $ do
  specify "produces the same reference documentation as before" $
    pureGoldenReferenceDocumentation path progname (settingsParser @a)

pureGoldenReferenceDocumentation :: FilePath -> String -> Parser a -> GoldenTest Text
pureGoldenReferenceDocumentation path progname parser =
  pureGoldenTextFile path $
    renderChunksText With24BitColours $
      renderReferenceDocumentation progname $
        parserDocs parser
