{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OptEnvConf.Test
  ( -- * Lint test
    settingsLintSpec,
    parserLintSpec,
    parserLintTest,

    -- * Parse tests
    settingsParserArgsTest,
    parserArgsTest,
    settingsParserEnvTest,
    parserEnvTest,
    settingsParserConfTest,
    parserConfTest,
    settingsParserTest,
    parserTest,

    -- * Reference documentation
    goldenSettingsReferenceDocumentationSpec,
    goldenParserReferenceDocumentationSpec,
    pureGoldenReferenceDocumentation,

    -- * Nix options
    goldenSettingsNixOptionsSpec,
    goldenParserNixOptionsSpec,
    pureGoldenNixOptions,
    module OptEnvConf,
  )
where

import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.Args
import OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error
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

settingsParserArgsTest :: (Show a, Eq a, HasParser a) => [String] -> a -> IO ()
settingsParserArgsTest = parserArgsTest settingsParser

parserArgsTest :: (Show a, Eq a) => Parser a -> [String] -> a -> IO ()
parserArgsTest parser args = parserTest parser args [] Nothing

settingsParserEnvTest :: (Show a, Eq a, HasParser a) => [(String, String)] -> a -> IO ()
settingsParserEnvTest = parserEnvTest settingsParser

parserEnvTest :: (Show a, Eq a) => Parser a -> [(String, String)] -> a -> IO ()
parserEnvTest parser envVars = parserTest parser [] envVars Nothing

settingsParserConfTest :: (Show a, Eq a, HasParser a) => JSON.Object -> a -> IO ()
settingsParserConfTest = parserConfTest settingsParser

parserConfTest :: (Show a, Eq a) => Parser a -> JSON.Object -> a -> IO ()
parserConfTest parser obj = parserTest parser [] [] (Just obj)

settingsParserTest :: (Show a, Eq a, HasParser a) => [String] -> [(String, String)] -> Maybe JSON.Object -> a -> IO ()
settingsParserTest = parserTest settingsParser

parserTest :: (Show a, Eq a) => Parser a -> [String] -> [(String, String)] -> Maybe JSON.Object -> a -> IO ()
parserTest parser args envVars mObject expected = do
  errOrActual <- runParserOn Nothing parser (parseArgs args) (EnvMap.parse envVars) mObject
  case errOrActual of
    Left errs -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors errs
    Right actual -> actual `shouldBe` expected

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

goldenSettingsNixOptionsSpec :: forall a. (HasCallStack) => (HasParser a) => FilePath -> Spec
goldenSettingsNixOptionsSpec path = withFrozenCallStack $ goldenParserNixOptionsSpec (settingsParser @a) path

goldenParserNixOptionsSpec :: (HasCallStack) => Parser a -> FilePath -> Spec
goldenParserNixOptionsSpec parser path = withFrozenCallStack $ do
  specify "produces the nix options as before" $
    pureGoldenNixOptions path parser

pureGoldenNixOptions :: FilePath -> Parser a -> GoldenTest Text
pureGoldenNixOptions path parser =
  pureGoldenTextFile path $
    renderParserNixOptions parser
