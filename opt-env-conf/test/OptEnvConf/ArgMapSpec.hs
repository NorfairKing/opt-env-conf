module OptEnvConf.ArgMapSpec (spec) where

import qualified Data.List.NonEmpty as NE
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..), Opt (..))
import qualified OptEnvConf.ArgMap as AM
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
          { argMapOpts = [OptArg "-"],
            argMapLeftovers = []
          }

    it "parses anything after -- as leftovers" $
      forAllValid $ \as ->
        forAllValid $ \bs ->
          argMapLeftovers (AM.parse (as ++ ["--"] ++ bs)) `shouldBe` bs

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
            { argMapOpts = [OptSwitch (DashedShort c)],
              argMapLeftovers = []
            }

    it "parses any string with two dashes and no argument as a switch" $
      forAllValid $ \s ->
        AM.parse ["--" <> NE.toList s]
          `shouldBe` ArgMap
            { argMapOpts = [OptSwitch (DashedLong s)],
              argMapLeftovers = []
            }

    it "parses any string with a dash and an argument as an option" $
      forAllValid $ \c ->
        forAllValid $ \o ->
          AM.parse [['-', c], o]
            `shouldBe` ArgMap
              { argMapOpts = [OptOption (DashedShort c) o],
                argMapLeftovers = []
              }

    it "parses any string with two dashes and an argument as an option" $
      forAllValid $ \s ->
        forAllValid $ \o ->
          AM.parse ["--" <> NE.toList s, o]
            `shouldBe` ArgMap
              { argMapOpts = [OptOption (DashedLong s) o],
                argMapLeftovers = []
              }
