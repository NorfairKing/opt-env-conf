{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.CompleterSpec (spec) where

import OptEnvConf.Completer
import Path
import Path.IO
import Test.Syd
import Test.Syd.Path

spec :: Spec
spec = do
  sequential $ tempDirSpec "opt-env-conf" $ do
    let setupExampleDir tdir = do
          -- File
          exampleFile1 <- resolveFile tdir "foo.txt"
          writeFile (fromAbsFile exampleFile1) ""
          -- File in dir
          exampleDir <- resolveDir tdir "bar"
          createDir exampleDir
          exampleFile2 <- resolveFile exampleDir "quux.txt"
          writeFile (fromAbsFile exampleFile2) ""
          -- Hidden file
          hiddenFile <- resolveFile tdir ".hidden.txt"
          writeFile (fromAbsFile hiddenFile) ""
          -- Hidden dir
          hiddenDir <- resolveDir tdir ".hidden"
          createDir hiddenDir

    it "can complete a file argument" $ \tdir ->
      withCurrentDir tdir $ do
        setupExampleDir tdir

        unCompleter filePath "" `shouldReturn` ["foo.txt", "bar"]
        unCompleter filePath "f" `shouldReturn` ["foo.txt"]
        unCompleter filePath "b" `shouldReturn` ["bar"]
        unCompleter filePath "q" `shouldReturn` []

    it "can complete a directory argument" $ \tdir ->
      withCurrentDir tdir $ do
        setupExampleDir tdir

        unCompleter directoryPath "" `shouldReturn` ["bar"]
        unCompleter directoryPath "f" `shouldReturn` []
