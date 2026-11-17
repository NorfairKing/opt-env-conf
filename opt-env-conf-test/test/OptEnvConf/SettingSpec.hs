{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.SettingSpec (spec) where

import Autodocodec
import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import OptEnvConf.Setting
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "confWith" $ do
    it "parses an Int that its encoder rendered, back to the same Int" $
      forAllValid $ \(i :: Int) ->
        case settingConfigVals (completeBuilder (confWith "key" codec)) of
          Just (ConfigValSetting {..} :| []) -> case configValSettingEncoder of
            Nothing -> expectationFailure "Expected an encoder."
            Just encode ->
              JSON.parseEither (parseJSONVia configValSettingCodec) (encode i)
                `shouldBe` Right (Just i)
          _ -> expectationFailure "Expected exactly one config value."

    it "parses a list of Text that its encoder rendered, back to the same list" $
      forAllValid $ \(ts :: [Text]) ->
        case settingConfigVals (completeBuilder (confWith "key" codec)) of
          Just (ConfigValSetting {..} :| []) -> case configValSettingEncoder of
            Nothing -> expectationFailure "Expected an encoder."
            Just encode ->
              JSON.parseEither (parseJSONVia configValSettingCodec) (encode ts)
                `shouldBe` Right (Just ts)
          _ -> expectationFailure "Expected exactly one config value."

  describe "confWith'" $
    it "parses an Int that its encoder rendered with a codec that reads null as 0, back to the same Int" $
      forAllValid $ \(i :: Int) ->
        case settingConfigVals (completeBuilder (confWith' "key" (dimapCodec (Just . fromMaybe 0) id (codec :: JSONCodec (Maybe Int))))) of
          Just (ConfigValSetting {..} :| []) -> case configValSettingEncoder of
            Nothing -> expectationFailure "Expected an encoder."
            Just encode ->
              JSON.parseEither (parseJSONVia configValSettingCodec) (encode i)
                `shouldBe` Right (Just i)
          _ -> expectationFailure "Expected exactly one config value."

  describe "decodeOnlyConfWith" $
    it "has no encoder" $
      case settingConfigVals (completeBuilder (decodeOnlyConfWith "key" (succ <$> (codec :: JSONCodec Int)))) of
        Just (ConfigValSetting {..} :| []) -> case configValSettingEncoder of
          Nothing -> pure ()
          Just _ -> expectationFailure "Expected no encoder."
        _ -> expectationFailure "Expected exactly one config value."
