module OptEnvConf.EnvMapSpec (spec) where

import qualified OptEnvConf.EnvMap as EM
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseEnvMap" $ do
    it "produces valid EnvMaps" $
      producesValid EM.parse
