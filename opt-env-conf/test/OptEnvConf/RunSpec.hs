module OptEnvConf.RunSpec (spec) where

import Control.Applicative
import Data.Aeson as JSON (Object)
import Data.GenValidity.Aeson ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import OptEnvConf
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..))
import qualified OptEnvConf.ArgMap as ArgMap
import OptEnvConf.ArgMap.Gen ()
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Parser
import OptEnvConf.Run
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "runParser" $ do
    describe "Pure" $ do
      it "can parse a pure value from anything" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \expected ->
              shouldParse (pure expected) ArgMap.empty env mConf (expected :: Int)

    describe "Fmap" $ do
      it "can parse a mapped value from anything" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let expected = succ i
              shouldParse (ParserFmap succ $ pure i) ArgMap.empty env mConf (expected :: Int)

    describe "Ap" $ do
      it "can parse two values with ap" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = (,) <$> pure (succ i) <*> pure i
              let expected = (succ i, i :: Int)
              shouldParse p ArgMap.empty env mConf expected

    describe "Empty" $ do
      it "can fail to parse an empty value" $
        forAllValid $ \env ->
          forAllValid $ \mConf -> do
            let p = empty :: Parser Int
            shouldFail p ArgMap.empty env mConf (ParseErrorEmpty :| [])

    describe "Alt" $ do
      it "can parse a Left value with Alt" $ do
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = (Left <$> pure i) <|> (Right <$> pure (succ i))
              let expected = Left (i :: Int)
              shouldParse p ArgMap.empty env mConf expected

      it "can parse a Right value with Alt" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = empty `ParserAlt` (Right <$> pure i)
              let expected = Right (i :: Int) :: Either Int Int
              shouldParse p ArgMap.empty env mConf expected

    describe "Many" $ do
      it "can pass many args" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = ArgMap.empty {argMapArgs = ls}
              let p = many $ strArgument []
              let expected = ls
              shouldParse p args env mConf expected

    describe "Some" $ do
      it "fails to parse zero args" $
        forAllValid $ \args' ->
          forAllValid $ \env ->
            forAllValid $ \mConf -> do
              let args = args' {argMapArgs = []}
              let p = some $ strArgument []
              shouldFail p args env mConf (ParseErrorEmpty :| [])

      it "can parse some args" $
        forAllValid $ \args' ->
          forAllValid $ \env ->
            forAllValid $ \mConf ->
              forAllValid $ \ls -> do
                let args = args' {argMapArgs = NE.toList ls}
                let p = some $ strArgument []
                let expected = NE.toList ls
                shouldParse p args env mConf expected

    describe "OptionalFirst" $ do
      pure ()

    describe "RequiredFirst" $ do
      pure ()

    describe "Arg" $ do
      it "can parse a single arg" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \arg -> do
              let args = ArgMap.empty {argMapArgs = [arg]}
              let p = strArgument []
              let expected = arg
              shouldParse p args env mConf expected

    describe "Opt" $ do
      pure ()

    describe "EnvVar" $ do
      pure ()

    describe "Config" $ do
      pure ()

shouldParse ::
  (Show a, Eq a) =>
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  a ->
  IO ()
shouldParse p args env mConf expected =
  case runParserPure p args env mConf of
    Left err -> expectationFailure $ show err
    Right (actual, _) -> actual `shouldBe` expected

shouldFail ::
  (Show a, Eq a) =>
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  NonEmpty ParseError ->
  IO ()
shouldFail p args env mConf expected =
  case runParserPure p args env mConf of
    Left err -> err `shouldBe` expected
    Right (actual, _) -> expectationFailure $ show actual
