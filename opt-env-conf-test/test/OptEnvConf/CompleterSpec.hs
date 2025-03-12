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

        let c s l = unCompleter filePath s `shouldReturn` l

        c "" ["foo.txt", "bar"]
        c "f" ["foo.txt"]
        c "b" ["bar"]
        c "q" []
        c "." [".hidden.txt", ".hidden"]
        c "./" ["./foo.txt", "./bar"]
        c "./." ["./.hidden.txt", "./.hidden"]

    it "can complete a directory argument" $ \tdir ->
      withCurrentDir tdir $ do
        setupExampleDir tdir

        let c s l = unCompleter directoryPath s `shouldReturn` l

        c "" ["bar"]
        c "f" []
        c "." [".hidden"]
        c "./" ["./bar"]
        c "./." ["./.hidden"]
