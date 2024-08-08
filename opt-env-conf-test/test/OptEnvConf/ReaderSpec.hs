{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.ReaderSpec (spec) where

import Data.GenValidity.Text ()
import Data.List.NonEmpty (NonEmpty (..))
import OptEnvConf.Reader
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "renderCommaSeparated" $
    it "escapes commas and backslashes" $
      renderCommaSeparated ("hi" :| [",", "\\", "ho"])
        `shouldBe` "hi,\\,,\\\\,ho"

  describe "parseCommaSeparated" $ do
    it "roundtrips with renderCommaSeparated starting from Strings" $
      forAllValid $ \s ->
        renderCommaSeparated (parseCommaSeparated s)
          `shouldBe` s
    it "roundtrips with renderCommaSeparated starting from lists" $
      forAllValid $ \l ->
        parseCommaSeparated (renderCommaSeparated l) `shouldBe` l

  describe "commaSeparated" $ do
    it "can parse these two ints" $
      runReader (commaSeparated auto) "1,2"
        `shouldBe` Right [1, 2 :: Int]

  describe "commaSeparatedList" $ do
    it "can parse these two ints" $
      runReader (commaSeparatedList auto) "3, 4"
        `shouldBe` Right [3, 4 :: Int]

  describe "commaSeparatedSet" $ do
    it "can parse these two ints" $
      runReader (commaSeparatedSet auto) "5,  6"
        `shouldBe` Right [5, 6 :: Int]

  describe "maybeReader" $
    it "can parse a bool using this example reader" $ do
      let r = maybeReader $ \case
            "true" -> Just True
            "false" -> Just False
            _ -> Nothing
      runReader r "true" `shouldBe` Right True
      runReader r "false" `shouldBe` Right False
      runReader r "yes" `shouldBe` Left "Unparseable value: \"yes\""

  describe "viaStringCodec" $
    it "can parse a string via its string codec" $
      runReader viaStringCodec "hi" `shouldBe` Right ("hi" :: String)
