{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.CheckSpec (spec) where

import Data.GenValidity.Aeson ()
import OptEnvConf
import OptEnvConf.Args as Args
import OptEnvConf.Args.Gen ()
import OptEnvConf.Check
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Terminal (getTerminalCapabilitiesFromHandle)
import System.IO
import Test.Syd

spec :: Spec
spec = sequential $
  doNotRandomiseExecutionOrder $
    describe "runSettingsCheckOn" $ do
      let string :: Reader String
          string = str
      it "is succesful on the trivial parser" $ do
        let p = pure ()
        stderrTc <- getTerminalCapabilitiesFromHandle stderr
        checkResult <- runSettingsCheckOn allCapabilities stderrTc p Args.emptyArgs EnvMap.empty Nothing
        checkResult
          `shouldSatisfy` ( \case
                              CheckSucceeded () -> True
                              _ -> False
                          )

      it "fails when a required setting is missing" $ do
        let p = setting [argument, reader string] :: Parser String
        stderrTc <- getTerminalCapabilitiesFromHandle stderr
        checkResult <- runSettingsCheckOn allCapabilities stderrTc p Args.emptyArgs EnvMap.empty Nothing
        checkResult
          `shouldSatisfy` ( \case
                              CheckFailed _ -> True
                              _ -> False
                          )

      it "says incapable when a required capability is missing" $ do
        let p = checkWithRequiredCapability readSecretCapability $ checkMapEither (const (Left "failed")) $ setting [argument, reader string] :: Parser String
        stderrTc <- getTerminalCapabilitiesFromHandle stderr
        checkResult <- runSettingsCheckOn (disableCapability (Capability "read-secret") allCapabilities) stderrTc p ["arg"] EnvMap.empty Nothing
        checkResult
          `shouldSatisfy` ( \case
                              CheckIncapable _ -> True
                              _ -> False
                          )

      it "says failed when a required capability is available" $ do
        let p = checkWithRequiredCapability readSecretCapability $ checkMapEither (const (Left "failed")) $ setting [argument, reader string] :: Parser String
        stderrTc <- getTerminalCapabilitiesFromHandle stderr
        checkResult <- runSettingsCheckOn allCapabilities stderrTc p ["arg"] EnvMap.empty Nothing
        checkResult
          `shouldSatisfy` ( \case
                              CheckFailed _ -> True
                              _ -> False
                          )

      it "says failed when a required capability is missing but another setting failed" $ do
        let p :: Parser (String, String)
            p =
              (,)
                <$> checkWithRequiredCapability readSecretCapability (checkMapEither (const (Left "failed")) $ setting [argument, reader string])
                <*> checkMapEither (const (Left "failed")) (setting [argument, reader string])

        stderrTc <- getTerminalCapabilitiesFromHandle stderr
        checkResult <- runSettingsCheckOn (disableCapability (Capability "read-secret") allCapabilities) stderrTc p ["arg1", "arg2"] EnvMap.empty Nothing
        checkResult
          `shouldSatisfy` ( \case
                              CheckFailed _ -> True
                              _ -> False
                          )
