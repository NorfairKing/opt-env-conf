{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.CheckSpec (spec) where

import Data.Aeson as JSON
import Data.GenValidity.Aeson ()
import OptEnvConf
import OptEnvConf.Args
import OptEnvConf.Args.Gen ()
import OptEnvConf.Check
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Parser
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 10) . modifyMaxSize (* 10) $ do
  -- Pure
  describe "pure" $ do
    checkMatchesRunProp (pure ())

  describe "single setting" $ do
    describe "argument" $ do
      let p :: Parser String
          p =
            setting
              [ argument,
                reader str
              ]
      checkMatchesRunArgsProp p ["arg"]
      checkMatchesRunArgsProp p []
      checkMatchesRunProp p

    describe "option" $ do
      let p :: Parser String
          p =
            setting
              [ option,
                long "foo",
                short 'f',
                reader str
              ]

      checkMatchesRunArgsProp p ["--foo", "foo"]
      checkMatchesRunArgsProp p ["-f", "foo"]
      checkMatchesRunArgsProp p ["--foo"]
      checkMatchesRunArgsProp p ["-f"]
      checkMatchesRunArgsProp p []
      checkMatchesRunProp p

    describe "switch" $ do
      let p :: Parser Bool
          p =
            setting
              [ switch True,
                value False,
                long "foo"
              ]

      checkMatchesRunArgsProp p ["--foo"]
      checkMatchesRunArgsProp p []
      checkMatchesRunProp p

    describe "env" $ do
      let p :: Parser String
          p =
            setting
              [ env "FOO",
                reader str
              ]

      checkMatchesRunEnvProp p [("FOO", "bar")]
      checkMatchesRunEnvProp p []
      checkMatchesRunProp p

    describe "conf" $ do
      let p :: Parser String
          p =
            setting
              [ conf "foo"
              ]

      checkMatchesRunConfProp p (Just [("foo", "bar")])
      checkMatchesRunConfProp p Nothing
      checkMatchesRunProp p

      pure ()

  pure ()

checkMatchesRunProp :: Parser a -> Spec
checkMatchesRunProp p = do
  it "matches run" $
    forAllValid $ \args ->
      forAllValid $ \env ->
        forAllValid $ \mConf ->
          checkMatchesRunTest p args env mConf

checkMatchesRunArgsProp :: Parser a -> Args -> Spec
checkMatchesRunArgsProp p args = do
  it "matches run" $
    forAllValid $ \env ->
      forAllValid $ \mConf ->
        checkMatchesRunTest p args env mConf

checkMatchesRunEnvProp :: Parser a -> EnvMap.EnvMap -> Spec
checkMatchesRunEnvProp p env = do
  it "matches run" $
    forAllValid $ \args ->
      forAllValid $ \mConf ->
        checkMatchesRunTest p args env mConf

checkMatchesRunConfProp :: Parser a -> Maybe JSON.Object -> Spec
checkMatchesRunConfProp p mConf = do
  it "matches run" $
    forAllValid $ \args ->
      forAllValid $ \env ->
        checkMatchesRunTest p args env mConf

checkMatchesRunArgsSpec :: Parser a -> Args -> Spec
checkMatchesRunArgsSpec p args = do
  checkMatchesRunSpec p args EnvMap.empty Nothing

checkMatchesRunSpec :: Parser a -> Args -> EnvMap.EnvMap -> Maybe JSON.Object -> Spec
checkMatchesRunSpec p args env mConf =
  it "matches run" $
    checkMatchesRunTest p args env mConf

checkMatchesRunTest :: Parser a -> Args -> EnvMap.EnvMap -> Maybe JSON.Object -> IO ()
checkMatchesRunTest p args env mConf = do
  runResult <- runParserOn Nothing p args env mConf
  checkResult <- runSettingsCheckOn Nothing p args env mConf

  case (checkResult, runResult) of
    (Nothing, Right _) -> pure ()
    (Just checkErrs, Left parseErrs) -> parseErrs `shouldBe` checkErrs
    (Nothing, Left parseErrs) -> expectationFailure $ "Expected success but got parse errors: " ++ show parseErrs
    (Just checkErrs, Right _) -> expectationFailure $ "Expected parse errors but got success: " ++ show checkErrs
