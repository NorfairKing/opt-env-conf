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
    pure ()
