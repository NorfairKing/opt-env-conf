{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.ArgsSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import OptEnvConf.Args
import OptEnvConf.Args.Gen ()
import Test.QuickCheck hiding (Args)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseArg" $ do
    it "produces valid args" $
      producesValid parseArg
    it "roundtrips with renderArg" $
      forAllValid $ \s ->
        renderArg (parseArg s) `shouldBe` s

  describe "parseArgs" $ do
    it "produces valid Args" $
      producesValid parseArgs

    let annoyingStrings :: Gen [String]
        annoyingStrings = genListOf $ genListOf $ oneof [genValid, pure '-']
    it "produces valid Argss for annoying strings" $
      forAll annoyingStrings $
        shouldBeValid . parseArgs

    it "parses empty args as an empty arg map" $
      parseArgs [] `shouldBe` emptyArgs

  describe "renderDashed" $ do
    it "roundtrips with parseArg for long dashed" $
      forAllValid $ \n ->
        parseArg (renderDashed (DashedLong n)) `shouldBe` ArgDashed True n

    it "roundtrips with parseArg for short dashed" $
      forAllValid $ \c ->
        parseArg (renderDashed (DashedShort c)) `shouldBe` ArgDashed False (c :| [])

  describe "consumeArgument" $ do
    it "does not consume anything if there is nothing to consume" $
      consumeArgument [] `shouldBe` []
    it "consumes any argument if there is only one" $
      forAllValid $ \a ->
        consumeArgument [a]
          `shouldBe` [(renderArg a, [])]

  describe "consumeOption" $ do
    it "fails to consume if there are no dasheds" $
      forAllValid $ \as ->
        consumeOption [] as `shouldBe` Nothing
    it "fails to consume if there are no arguments" $
      forAllValid $ \ds ->
        consumeOption ds [] `shouldBe` Nothing
    it "does not consume a mismatched option" $
      consumeOption ["--foo"] ["--bar", "quux"] `shouldBe` Nothing
    it "consumes a short option" $
      consumeOption ["-f"] ["-f", "foo"] `shouldBe` Just ("foo", [])
    it "consumes a long option" $
      consumeOption ["--foo"] ["--foo", "foo"] `shouldBe` Just ("foo", [])
    it "consumes an option at the front first" $
      consumeOption ["-f"] ["-f", "foo", "-f", "bar"] `shouldBe` Just ("foo", ["-f", "bar"])
    it "consumes a folded option" $
      consumeOption ["-f"] ["-vf", "foo"] `shouldBe` Just ("foo", ["-v"])

  describe "consumeSwitch" $ do
    it "fails to consume if there are no dasheds" $
      forAllValid $ \as ->
        consumeSwitch [] as `shouldBe` Nothing
    it "fails to consume if there are no arguments" $
      forAllValid $ \ds ->
        consumeSwitch ds [] `shouldBe` Nothing

    it "does not consume a mismatched switch" $
      consumeSwitch ["--foo"] ["--bar"] `shouldBe` Nothing
    it "consumes a short switch if there are no other args" $
      consumeSwitch ["-v"] ["-v"] `shouldBe` Just []
    it "consumes a long switch if there are no other args" $
      consumeSwitch ["--verbose"] ["--verbose"] `shouldBe` Just []
    it "consumes a switch at the front first" $
      consumeSwitch ["-a", "-b"] ["-a", "-b"] `shouldBe` Just ["-b"]
    it "consumes a folded switch at the front first" $
      consumeSwitch ["-a", "-b"] ["-ab"] `shouldBe` Just ["-b"]
