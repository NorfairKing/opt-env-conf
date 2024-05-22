module OptEnvConf.CompletionSpec (spec) where

import OptEnvConf.Completion
import OptEnvConf.Parser
import OptEnvConf.Setting
import Test.Syd

spec :: Spec
spec = do
  describe "pureCompletionQuery" $ do
    it "can complete a short switch from a single dash" $
      pureCompletionQuery (setting [short 'e']) 1 ["-"]
        `shouldBe` ["-e"]
    it "can complete a long switch from a single dash" $
      pureCompletionQuery (setting [long "example"]) 1 ["-"]
        `shouldBe` ["--example"]
    it "can complete a long switch from a double dash" $
      pureCompletionQuery (setting [long "example"]) 1 ["--"]
        `shouldBe` ["--example"]
