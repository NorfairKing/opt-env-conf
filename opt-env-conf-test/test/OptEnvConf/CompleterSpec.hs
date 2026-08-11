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
        let c :: (HasCallStack) => String -> [CompletionResult] -> TestDef '[Path Abs Dir] ()
            c s l =
              withFrozenCallStack $
                itWithOuter (unwords ["can complete", show s, "to", show (map completionResultValue l)]) $ \tdir ->
                  withCurrentDir tdir $
                    unCompleter filePath s `shouldReturn` l

        c "" [fileR "config.yaml", fileR "foo.txt", dirR "bar/"]
        c "f" [fileR "foo.txt"]
        c "b" [dirR "bar/"]
        c "bar" [fileR "bar/quux.txt", dirR "bar/", dirR "bar/deep/"]
        c "c" [fileR "config.yaml"]
        c "q" []
        c "." [fileR ".hidden.txt", dirR ".hidden/"]
        c "./" [fileR "./config.yaml", fileR "./foo.txt", dirR "./bar/"]
        c "././" [fileR "././config.yaml", fileR "././foo.txt", dirR "././bar/"]
        c "./." [fileR "./.hidden.txt", dirR "./.hidden/"]
        c "./bar" [fileR "./bar/quux.txt", dirR "./bar/", dirR "./bar/deep/"]

        -- Deeper nesting.
        -- Directories end in /, files do not.  This convention is how
        -- shells decide whether to append a trailing space after a
        -- completion.
        c "bar/" [fileR "bar/quux.txt", dirR "bar/deep/"]
        c "bar/d" [dirR "bar/deep/"]
        c "bar/deep" [fileR "bar/deep/gold.txt", dirR "bar/deep/"]
        c "bar/deep/" [fileR "bar/deep/gold.txt"]
        c "bar/q" [fileR "bar/quux.txt"]

        -- Parent directory paths (..)
        -- These tests run from inside bar/ so that .. refers to the
        -- test root directory.
        itWithOuter "can complete \"..\" to parent directory contents" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter filePath ".."
            results
              `shouldBe` [ fileR "../config.yaml",
                           fileR "../foo.txt",
                           dirR "../bar/"
                         ]
        itWithOuter "can complete \"../\" to parent directory contents" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter filePath "../"
            results
              `shouldBe` [ fileR "../config.yaml",
                           fileR "../foo.txt",
                           dirR "../bar/"
                         ]
        itWithOuter "can complete \"../f\" to matching parent files" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter filePath "../f"
            results
              `shouldBe` [fileR "../foo.txt"]
        itWithOuter "can complete \"../bar/\" to sibling dir contents" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter filePath "../bar/"
            results
              `shouldBe` [ fileR "../bar/quux.txt",
                           dirR "../bar/deep/"
                         ]

        -- Parent directory in the middle of a path
        itWithOuter "can complete \"bar/../\" to current dir contents" $ \tdir ->
          withCurrentDir tdir $ do
            results <- unCompleter filePath "bar/../"
            results
              `shouldBe` [ fileR "bar/../config.yaml",
                           fileR "bar/../foo.txt",
                           dirR "bar/../bar/"
                         ]
        itWithOuter "can complete \"bar/../f\" to matching files" $ \tdir ->
          withCurrentDir tdir $ do
            results <- unCompleter filePath "bar/../f"
            results
              `shouldBe` [fileR "bar/../foo.txt"]
        itWithOuter "can complete \"../bar/deep/../\" to bar/ contents" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter filePath "../bar/deep/../"
            results
              `shouldBe` [ fileR "../bar/deep/../quux.txt",
                           dirR "../bar/deep/../deep/"
                         ]

        -- Absolute paths
        itWithOuter "can complete absolute paths" $ \tdir ->
          withCurrentDir tdir $ do
            let absPrefix = fromAbsDir tdir
            results <- unCompleter filePath absPrefix
            results
              `shouldBe` [ fileR (absPrefix <> "config.yaml"),
                           fileR (absPrefix <> "foo.txt"),
                           dirR (absPrefix <> "bar/")
                         ]

      describe "directoryPath" $ do
        let c :: (HasCallStack) => String -> [CompletionResult] -> TestDef '[Path Abs Dir] ()
            c s l = withFrozenCallStack $
              itWithOuter (unwords ["can complete", show s, "to", show (map completionResultValue l)]) $ \tdir ->
                withCurrentDir tdir $
                  unCompleter directoryPath s `shouldReturn` l

        c "" [dirR "bar/"]
        c "b" [dirR "bar/"]
        c "f" []
        c "." [dirR ".hidden/"]
        c "./" [dirR "./bar/"]
        c "./." [dirR "./.hidden/"]
        c "././" [dirR "././bar/"]
        c "./." [dirR "./.hidden/"]
        c "./bar" [dirR "./bar/", dirR "./bar/deep/"]

        -- Deeper nesting.
        -- Only directories are returned, never files.
        c "bar/" [dirR "bar/", dirR "bar/deep/"]
        c "bar/d" [dirR "bar/deep/"]
        c "bar/deep" [dirR "bar/deep/"]

        -- Parent directory paths (..)
        itWithOuter "can complete \"..\" to parent directories" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter directoryPath ".."
            results
              `shouldBe` [dirR "../bar/"]
        itWithOuter "can complete \"../\" to parent directories" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter directoryPath "../"
            results
              `shouldBe` [dirR "../bar/"]
        itWithOuter "can complete \"../b\" to matching parent directories" $ \tdir -> do
          subdir <- resolveDir tdir "bar"
          withCurrentDir subdir $ do
            results <- unCompleter directoryPath "../b"
            results
              `shouldBe` [dirR "../bar/"]

        -- Parent directory in the middle of a path
        itWithOuter "can complete \"bar/../\" to current dir directories" $ \tdir ->
          withCurrentDir tdir $ do
            results <- unCompleter directoryPath "bar/../"
            results
              `shouldBe` [dirR "bar/../bar/"]
        itWithOuter "can complete \"bar/../b\" to matching directories" $ \tdir ->
          withCurrentDir tdir $ do
            results <- unCompleter directoryPath "bar/../b"
            results
              `shouldBe` [dirR "bar/../bar/"]

      describe "filePathWithExtension" $ do
        let c :: (HasCallStack) => String -> [CompletionResult] -> TestDef '[Path Abs Dir] ()
            c s l = withFrozenCallStack $
              itWithOuter (unwords ["can complete", show s, "to", show (map completionResultValue l)]) $ \tdir ->
                withCurrentDir tdir $
                  unCompleter (filePathWithExtension ".yaml") s `shouldReturn` l

        c "" [fileR "config.yaml", dirR "bar/"]
        c "c" [fileR "config.yaml"]
        c "b" [dirR "bar/"]
        c "bar" [dirR "bar/", dirR "bar/deep/"]

      describe "filePathWithExtensions" $ do
        let c :: (HasCallStack) => String -> [CompletionResult] -> TestDef '[Path Abs Dir] ()
            c s l = withFrozenCallStack $
              itWithOuter (unwords ["can complete", show s, "to", show (map completionResultValue l)]) $ \tdir ->
                withCurrentDir tdir $
                  unCompleter (filePathWithExtensions [".txt", ".yaml"]) s `shouldReturn` l

        c "" [fileR "config.yaml", fileR "foo.txt", dirR "bar/"]
        c "bar/" [fileR "bar/quux.txt", dirR "bar/deep/"]

fileR :: String -> CompletionResult
fileR s =
  CompletionResult
    { completionResultValue = s,
      completionResultFinality = CompletionFinal
    }

dirR :: String -> CompletionResult
dirR s =
  CompletionResult
    { completionResultValue = s,
      completionResultFinality = CompletionNotFinal
    }
