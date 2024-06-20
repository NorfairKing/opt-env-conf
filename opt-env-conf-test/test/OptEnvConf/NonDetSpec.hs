{-# LANGUAGE ApplicativeDo #-}

module OptEnvConf.NonDetSpec (spec) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import OptEnvConf.NonDet
import Test.Syd

spec :: Spec
spec = do
  describe "runNonDet" $ do
    it "can guard against previous values" $
      runNonDet
        ( do
            a <- liftListT [1, 2 :: Int]
            b <- liftListT [3, 4]
            guard $ even $ a + b
            pure (a, b)
        )
        `shouldBe` [(1, 3), (2, 4)]
    it "can recover from failures with <|>" $
      runNonDet
        ( do
            a <- liftListT [1, 2 :: Int]
            f <- liftListT [even] <|> liftListT [odd]
            guard $ f a
            pure a
        )
        `shouldBe` [1, 2]

  describe "runNonDetT" $ do
    it "can recover from underlying errors" $ do
      let f :: NonDetT (Either String) Int
          f = do
            i <- liftListT [1, 2]
            if odd i
              then do
                cutListT
                lift (Left "err")
              else pure i

      runNonDetT f `shouldBe` Right [2]
