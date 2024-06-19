module OptEnvConf.ArgMapSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (Arg (..), ArgMap (..), Dashed (..), Opt (..))
import qualified OptEnvConf.ArgMap as AM
import OptEnvConf.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseSingleArg" $
    it "produces valid args" $
      producesValid AM.parseSingleArg

  describe "AM.parse" $ do
    it "produces valid ArgMaps" $
      producesValid AM.parse

    let annoyingStrings :: Gen [String]
        annoyingStrings = genListOf $ genListOf $ oneof [genValid, pure '-']
    it "produces valid ArgMaps for annoying strings" $
      forAll annoyingStrings $
        shouldBeValid . AM.parse

    it "parses empty args as an empty arg map" $
      AM.parse [] `shouldBe` AM.empty

    it "treats '-' as an argument" $
      AM.parse ["-"]
        `shouldBe` ArgMap
          { argMapOpts = [OptArg "-"]
          }

    it "parses anything after -- as arguments" $
      forAllValid $ \as ->
        AM.parse ("--" : as) `shouldBe` ArgMap (map OptArg as)

    it "parses -a -b the same as -ab" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 ->
          forAllValid $ \b ->
            forAllValid $ \a -> do
              let actualArgs = concat [b, [['-', c1], ['-', c2]], a]
              let expectedArgs = concat [b, [['-', c1, c2]], a]
              context ("actual: " <> show actualArgs) $
                context ("expected: " <> show expectedArgs) $
                  AM.parse actualArgs `shouldBe` AM.parse expectedArgs

    it "parses any string with one dash and no argument as a switch" $
      forAllValid $ \c ->
        AM.parse [['-', c]]
          `shouldBe` ArgMap
            { argMapOpts = [OptSwitch (DashedShort c)]
            }

    it "parses any string with two dashes and no argument as a switch" $
      forAllValid $ \s ->
        AM.parse ["--" <> NE.toList s]
          `shouldBe` ArgMap
            { argMapOpts = [OptSwitch (DashedLong s)]
            }

    it "parses any string with a dash and an argument as an option" $
      forAllValid $ \c ->
        forAllValid $ \o ->
          AM.parse [['-', c], o]
            `shouldBe` ArgMap
              { argMapOpts = [OptOption (DashedShort c) o]
              }

    it "parses any string with two dashes and an argument as an option" $
      forAllValid $ \s ->
        forAllValid $ \o ->
          AM.parse ["--" <> NE.toList s, o]
            `shouldBe` ArgMap
              { argMapOpts = [OptOption (DashedLong s) o]
              }

  describe "renderDashed" $ do
    it "roundtrips with parseSingleArg for long dashed" $
      forAllValid $ \n ->
        AM.parseSingleArg (AM.renderDashed (DashedLong n)) `shouldBe` ArgDashed True n

    it "roundtrips with parseSingleArg for short dashed" $
      forAllValid $ \c ->
        AM.parseSingleArg (AM.renderDashed (DashedShort c)) `shouldBe` ArgDashed False (c :| [])
