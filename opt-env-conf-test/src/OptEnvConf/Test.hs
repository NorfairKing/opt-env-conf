{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.Test
  ( settingsLintSpec,
    parserLintSpec,
    parserLintTest,
    goldenSettingsReferenceDocumentationSpec,
    goldenParserReferenceDocumentationSpec,
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
settingsLintSpec = withFrozenCallStack $ parserLintSpec (settingsParser @a)

parserLintSpec :: forall a. (HasCallStack) => Parser a -> Spec
parserLintSpec parser =
  withFrozenCallStack $
    specify "pass the lint test" $
      parserLintTest parser

parserLintTest :: Parser a -> IO ()
parserLintTest parser =
  case lintParser parser of
    Nothing -> pure ()
    Just errs ->
      expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderLintErrors errs

goldenSettingsReferenceDocumentationSpec :: forall a. (HasCallStack) => (HasParser a) => FilePath -> String -> Spec
goldenSettingsReferenceDocumentationSpec path progname = withFrozenCallStack $ goldenParserReferenceDocumentationSpec (settingsParser @a) path progname

goldenParserReferenceDocumentationSpec :: (HasCallStack) => Parser a -> FilePath -> String -> Spec
goldenParserReferenceDocumentationSpec parser path progname = withFrozenCallStack $ do
  specify "produces the same reference documentation as before" $
    pureGoldenReferenceDocumentation path progname parser

pureGoldenReferenceDocumentation :: FilePath -> String -> Parser a -> GoldenTest Text
pureGoldenReferenceDocumentation path progname parser =
  pureGoldenTextFile path $
    renderChunksText With24BitColours $
      renderReferenceDocumentation progname $
        parserDocs parser
