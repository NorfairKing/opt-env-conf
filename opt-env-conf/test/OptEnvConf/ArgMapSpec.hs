module OptEnvConf.ArgMapSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import OptEnvConf.ArgMap
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseArgMap" $ do
    it "produces valid ArgMaps" $
      producesValid parseArgMap

    let annoyingStrings :: Gen [String]
        annoyingStrings = genListOf $ genListOf $ oneof [genValid, pure '-']
    it "produces valid ArgMaps for annoying strings" $
      forAll annoyingStrings $
        shouldBeValid . parseArgMap

    it "parses empty args as an empty arg map" $
      parseArgMap [] `shouldBe` emptyArgMap

    it "treats '-' as an argument" $
      parseArgMap ["-"]
        `shouldBe` ArgMap
          { argMapArgs = ["-"],
            argMapSwitches = [],
            argMapOptions = M.empty,
            argMapLeftovers = []
          }

    it "parses anything after -- as leftovers" $
      forAllValid $ \as ->
        forAllValid $ \bs ->
          argMapLeftovers (parseArgMap (as ++ ["--"] ++ bs)) `shouldBe` bs

    it "parses any string with one dash and no argument as a switch" $
      forAllValid $ \c ->
        parseArgMap ["-" <> NE.toList c]
          `shouldBe` ArgMap
            { argMapArgs = [],
              argMapSwitches = [DashedShort c],
              argMapOptions = M.empty,
              argMapLeftovers = []
            }

    it "parses any string with two dashes and no argument as a switch" $
      forAllValid $ \s ->
        parseArgMap ["--" <> NE.toList s]
          `shouldBe` ArgMap
            { argMapArgs = [],
              argMapSwitches = [DashedLong s],
              argMapOptions = M.empty,
              argMapLeftovers = []
            }

    it "parses any string with a dash and an argument as an option" $
      forAllValid $ \s ->
        forAllValid $ \o ->
          parseArgMap ["-" <> NE.toList s, o]
            `shouldBe` ArgMap
              { argMapArgs = [],
                argMapSwitches = [],
                argMapOptions = M.singleton (DashedShort s) (o :| []),
                argMapLeftovers = []
              }

    it "parses any string with two dashes and an argument as an option" $
      forAllValid $ \s ->
        forAllValid $ \o ->
          parseArgMap ["--" <> NE.toList s, o]
            `shouldBe` ArgMap
              { argMapArgs = [],
                argMapSwitches = [],
                argMapOptions = M.singleton (DashedLong s) (o :| []),
                argMapLeftovers = []
              }
