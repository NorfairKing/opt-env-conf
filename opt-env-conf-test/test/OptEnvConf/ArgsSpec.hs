module OptEnvConf.ArgsSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import OptEnvConf.Args
import OptEnvConf.Gen ()
import Test.QuickCheck hiding (Args)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseSingleArg" $
    it "produces valid args" $
      producesValid parseSingleArg

  describe "parse" $ do
    it "produces valid Argss" $
      producesValid parse

    let annoyingStrings :: Gen [String]
        annoyingStrings = genListOf $ genListOf $ oneof [genValid, pure '-']
    it "produces valid Argss for annoying strings" $
      forAll annoyingStrings $
        shouldBeValid . parse

    it "parses empty args as an empty arg map" $
      parse [] `shouldBe` empty

    it "treats '-' as an argument" $
      parse ["-"]
        `shouldBe` Args
          { argMapOpts = [OptArg "-"]
          }

    it "parses anything after -- as arguments" $
      forAllValid $ \as ->
        parse ("--" : as) `shouldBe` Args (map OptArg as)

    it "parses -a -b the same as -ab" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 ->
          forAllValid $ \b ->
            forAllValid $ \a -> do
              let actualArgs = concat [b, [['-', c1], ['-', c2]], a]
              let expectedArgs = concat [b, [['-', c1, c2]], a]
              context ("actual: " <> show actualArgs) $
                context ("expected: " <> show expectedArgs) $
                  parse actualArgs `shouldBe` parse expectedArgs

    it "parses any string with one dash and no argument as a switch" $
      forAllValid $ \c ->
        parse [['-', c]]
          `shouldBe` Args
            { argMapOpts = [OptSwitch (DashedShort c)]
            }

    it "parses any string with two dashes and no argument as a switch" $
      forAllValid $ \s ->
        parse ["--" <> NE.toList s]
          `shouldBe` Args
            { argMapOpts = [OptSwitch (DashedLong s)]
            }

    it "parses any string with a dash and an argument as an option" $
      forAllValid $ \c ->
        forAllValid $ \o ->
          parse [['-', c], o]
            `shouldBe` Args
              { argMapOpts = [OptOption (DashedShort c) o]
              }

    it "parses any string with two dashes and an argument as an option" $
      forAllValid $ \s ->
        forAllValid $ \o ->
          parse ["--" <> NE.toList s, o]
            `shouldBe` Args
              { argMapOpts = [OptOption (DashedLong s) o]
              }

  describe "renderDashed" $ do
    it "roundtrips with parseSingleArg for long dashed" $
      forAllValid $ \n ->
        parseSingleArg (renderDashed (DashedLong n)) `shouldBe` ArgDashed True n

    it "roundtrips with parseSingleArg for short dashed" $
      forAllValid $ \c ->
        parseSingleArg (renderDashed (DashedShort c)) `shouldBe` ArgDashed False (c :| [])
