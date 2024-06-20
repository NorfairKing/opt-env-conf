{-# LANGUAGE ApplicativeDo #-}

module OptEnvConf.NonDetSpec (spec) where

import Control.Monad
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

  pure ()
