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
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 10) . modifyMaxSize (* 10) $ do
  describe "pure" $ do
    checkMatchesRunProp (pure ())

  describe "empty" $ do
    checkMatchesRunProp (choice [])

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

  describe "ap" $
    describe "two strings" $ do
      let p :: Parser (String, String)
          p =
            (,)
              <$> setting
                [ argument,
                  reader str
                ]
              <*> setting
                [ option,
                  long "foo",
                  reader str
                ]
      checkMatchesRunArgsSpec p ["arg", "--foo", "foo"]
      checkMatchesRunArgsSpec p ["arg"]
      checkMatchesRunArgsSpec p ["--foo", "foo"]
      checkMatchesRunArgsSpec p []
      checkMatchesRunProp p

  pure ()

checkMatchesRunProp :: Parser a -> Spec
checkMatchesRunProp p = do
  it "matches run" $
    forAllValid $ \args ->
      forAllValid $ \envMap ->
        forAllValid $ \mConf ->
          checkMatchesRunTest p args envMap mConf

checkMatchesRunArgsProp :: Parser a -> Args -> Spec
checkMatchesRunArgsProp p args = do
  it "matches run" $
    forAllValid $ \envMap ->
      forAllValid $ \mConf ->
        checkMatchesRunTest p args envMap mConf

checkMatchesRunEnvProp :: Parser a -> EnvMap.EnvMap -> Spec
checkMatchesRunEnvProp p envMap = do
  it "matches run" $
    forAllValid $ \args ->
      forAllValid $ \mConf ->
        checkMatchesRunTest p args envMap mConf

checkMatchesRunConfProp :: Parser a -> Maybe JSON.Object -> Spec
checkMatchesRunConfProp p mConf = do
  it "matches run" $
    forAllValid $ \args ->
      forAllValid $ \envMap ->
        checkMatchesRunTest p args envMap mConf

checkMatchesRunArgsSpec :: Parser a -> Args -> Spec
checkMatchesRunArgsSpec p args = do
  checkMatchesRunSpec p args EnvMap.empty Nothing

checkMatchesRunSpec :: Parser a -> Args -> EnvMap.EnvMap -> Maybe JSON.Object -> Spec
checkMatchesRunSpec p args envMap mConf =
  it "matches run" $
    checkMatchesRunTest p args envMap mConf

checkMatchesRunTest :: Parser a -> Args -> EnvMap.EnvMap -> Maybe JSON.Object -> IO ()
checkMatchesRunTest p args envMap mConf = do
  runResult <- runParserOn Nothing p args envMap mConf
  checkResult <- runSettingsCheckOn Nothing p args envMap mConf

  case (checkResult, runResult) of
    (Nothing, Right _) -> pure ()
    (Just checkErrs, Left parseErrs) -> parseErrs `shouldBe` checkErrs
    (Nothing, Left parseErrs) -> expectationFailure $ "Expected success but got parse errors: " ++ show parseErrs
    (Just checkErrs, Right _) -> expectationFailure $ "Expected parse errors but got success: " ++ show checkErrs
