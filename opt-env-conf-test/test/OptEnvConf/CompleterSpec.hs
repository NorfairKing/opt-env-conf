{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.CompleterSpec (spec) where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf.Completer
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec = do
  let setupExampleDir tdir = do
        -- File
        exampleFile1 <- resolveFile tdir "foo.txt"
        writeFile (fromAbsFile exampleFile1) ""
        -- Dir here
        exampleDir <- resolveDir tdir "bar"
        createDir exampleDir
        -- File in dir
        exampleFile2 <- resolveFile exampleDir "quux.txt"
        writeFile (fromAbsFile exampleFile2) ""
        -- Dir in dir
        deeperDir <- resolveDir exampleDir "deep"
        createDir deeperDir
        -- File in dir in dir
        exampleFile3 <- resolveFile deeperDir "gold.txt"
        writeFile (fromAbsFile exampleFile3) ""
        -- Hidden file
        hiddenFile <- resolveFile tdir ".hidden.txt"
        writeFile (fromAbsFile hiddenFile) ""
        -- Hidden dir
        hiddenDir <- resolveDir tdir ".hidden"
        createDir hiddenDir

  -- These are read-only tests so we only need one dir for all of them
  sequential . doNotRandomiseExecutionOrder
    $ aroundAll
      ( \func -> withSystemTempDir "opt-env-conf-test" $ \tdir -> do
          setupExampleDir tdir
          func tdir
      )
    $ do
      describe "filePath" $ do
        let c :: (HasCallStack) => String -> [String] -> TestDef '[Path Abs Dir] ()
            c s l =
              withFrozenCallStack $
                itWithOuter (unwords ["can complete", show s, "to", show l]) $ \tdir ->
                  withCurrentDir tdir $
                    unCompleter filePath s `shouldReturn` l

        c "" ["foo.txt", "bar/"]
        c "f" ["foo.txt"]
        c "b" ["bar/"]
        c "bar" ["bar/quux.txt", "bar/", "bar/deep/"]
        c "q" []
        c "." [".hidden.txt", ".hidden/"]
        c "./" ["./foo.txt", "./bar/"]
        c "././" ["././foo.txt", "././bar/"]
        c "./." ["./.hidden.txt", "./.hidden/"]
        c "./bar" ["./bar/quux.txt", "./bar/", "./bar/deep/"]

      describe "directoryPath" $ do
        let c :: (HasCallStack) => String -> [String] -> TestDef '[Path Abs Dir] ()
            c s l = withFrozenCallStack $
              itWithOuter (unwords ["can complete", show s, "to", show l]) $ \tdir ->
                withCurrentDir tdir $
                  unCompleter directoryPath s `shouldReturn` l

        c "" ["bar/"]
        c "b" ["bar/"]
        c "f" []
        c "." [".hidden/"]
        c "./" ["./bar/"]
        c "./." ["./.hidden/"]
        c "././" ["././bar/"]
        c "./." ["./.hidden/"]
        c "./bar" ["./bar/", "./bar/deep/"]
