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
