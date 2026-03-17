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
        -- YAML file
        yamlFile <- resolveFile tdir "config.yaml"
        writeFile (fromAbsFile yamlFile) ""
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

        c "" ["foo.txt", "config.yaml", "bar/"]
        c "f" ["foo.txt"]
        c "b" ["bar/"]
        c "bar" ["bar/quux.txt", "bar/", "bar/deep/"]
        c "c" ["config.yaml"]
        c "q" []
        c "." [".hidden.txt", ".hidden/"]
        c "./" ["./foo.txt", "./config.yaml", "./bar/"]
        c "././" ["././foo.txt", "././config.yaml", "././bar/"]
        c "./." ["./.hidden.txt", "./.hidden/"]
        c "./bar" ["./bar/quux.txt", "./bar/", "./bar/deep/"]

        -- Deeper nesting.
        -- Directories end in /, files do not.  This convention is how
        -- shells decide whether to append a trailing space after a
        -- completion.
        c "bar/" ["bar/quux.txt", "bar/deep/"]
        c "bar/d" ["bar/deep/"]
        c "bar/deep" ["bar/deep/gold.txt", "bar/deep/"]
        c "bar/deep/" ["bar/deep/gold.txt"]
        c "bar/q" ["bar/quux.txt"]

        -- Absolute paths
        itWithOuter "can complete absolute paths" $ \tdir ->
          withCurrentDir tdir $ do
            let absPrefix = fromAbsDir tdir
            results <- unCompleter filePath absPrefix
            results `shouldBe` [absPrefix <> "foo.txt", absPrefix <> "config.yaml", absPrefix <> "bar/"]

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

        -- Deeper nesting.
        -- Only directories are returned, never files.
        c "bar/" ["bar/", "bar/deep/"]
        c "bar/d" ["bar/deep/"]
        c "bar/deep" ["bar/deep/"]

      describe "filePathWithExtension" $ do
        let c :: (HasCallStack) => String -> [String] -> TestDef '[Path Abs Dir] ()
            c s l = withFrozenCallStack $
              itWithOuter (unwords ["can complete", show s, "to", show l]) $ \tdir ->
                withCurrentDir tdir $
                  unCompleter (filePathWithExtension ".yaml") s `shouldReturn` l

        c "" ["config.yaml", "bar/"]
        c "c" ["config.yaml"]
        c "b" ["bar/"]
        c "bar" ["bar/", "bar/deep/"]

      describe "filePathWithExtensions" $ do
        let c :: (HasCallStack) => String -> [String] -> TestDef '[Path Abs Dir] ()
            c s l = withFrozenCallStack $
              itWithOuter (unwords ["can complete", show s, "to", show l]) $ \tdir ->
                withCurrentDir tdir $
                  unCompleter (filePathWithExtensions [".txt", ".yaml"]) s `shouldReturn` l

        c "" ["foo.txt", "config.yaml", "bar/"]
        c "bar/" ["bar/quux.txt", "bar/deep/"]
