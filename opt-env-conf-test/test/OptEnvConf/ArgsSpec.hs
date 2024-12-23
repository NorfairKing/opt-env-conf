{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module OptEnvConf.ArgsSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import OptEnvConf.Args
import OptEnvConf.Args.Gen ()
import Test.QuickCheck hiding (Args)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseArg" $ do
    it "produces valid args" $
      producesValid parseArg
    it "roundtrips with renderArg" $
      forAllValid $ \s ->
        renderArg (parseArg s) `shouldBe` s

  describe "parseArgs" $ do
    it "produces valid Args" $
      producesValid parseArgs

    let annoyingStrings :: Gen [String]
        annoyingStrings = genListOf $ genListOf $ oneof [genValid, pure '-']
    it "produces valid Argss for annoying strings" $
      forAll annoyingStrings $
        shouldBeValid . parseArgs

    it "parses empty args as an empty arg map" $
      parseArgs [] `shouldBe` emptyArgs

  describe "renderDashed" $ do
    it "roundtrips with parseArg for long dashed" $
      forAllValid $ \n ->
        parseArg (renderDashed (DashedLong n)) `shouldBe` ArgDashed True n

    it "roundtrips with parseArg for short dashed" $
      forAllValid $ \c ->
        parseArg (renderDashed (DashedShort c)) `shouldBe` ArgDashed False (c :| [])

  describe "consumeArgument" $ do
    it "does not consume anything if there is nothing to consume" $
      consumeArgument [] `shouldBe` [(Nothing, emptyArgs)]
    it "consumes a plain argument when there is one" $
      forAllValid $ \s ->
        let as = [Live (ArgPlain s)]
         in consumeArgument as
              `shouldBe` [(Just s, Args [Dead] []), (Nothing, as)]
    it "consumes a bare double-dash if it's the last argument" $
      forAllValid $ \befores ->
        let as = Args befores [Live ArgBareDoubleDash]
         in consumeArgument as
              `shouldBe` [ (Just "--", Args (befores ++ [Dead]) []),
                           (Nothing, as)
                         ]
    it "consumes any argument after a double-dash as an argument" $
      forAllValid $ \befores ->
        forAllValid $ \bareArg ->
          forAllValid $ \rest ->
            let as = Args befores (Live ArgBareDoubleDash : Live bareArg : rest)
             in consumeArgument as
                  `shouldBe` [ (Just (renderArg bareArg), Args befores (Live ArgBareDoubleDash : Dead : rest)),
                               (Nothing, as)
                             ]
    it "skips dead arguments" $
      forAllValid $ \befores ->
        forAllValid $ \afters ->
          consumeArgument (Args befores (Dead : afters)) `shouldBe` consumeArgument (Args (befores ++ [Dead]) afters)

    it "tries to consume dashed argument followed by a dead argument" $
      forAllValid $ \befores ->
        forAllValid $ \isLong ->
          forAllValid $ \cs ->
            let d = ArgDashed isLong cs
                args = Args befores [Live d, Dead]
             in consumeArgument args
                  `shouldBe` [ (Nothing, Args (befores ++ [Live d, Dead]) []),
                               (Just (renderArg d), Args (befores ++ [Dead]) [Dead])
                             ]

    it "tries to consume dashed argument followed by a live argument" $
      forAllValid $ \befores ->
        forAllValid $ \isLong ->
          forAllValid $ \cs ->
            forAll (genValid `suchThat` (/= ArgBareDoubleDash)) $ \arg ->
              let d = ArgDashed isLong cs
                  args = Args befores [Live d, Live arg]
               in context (ppShow args) $
                    consumeArgument args
                      `shouldBe` [ (Nothing, Args (befores ++ [Live d, Live arg]) []),
                                   -- Consuming the value (dashed is a switch) is
                                   -- more likely than consuming the dashed as an
                                   -- argument
                                   (Just (renderArg arg), Args (befores ++ [Live d, Dead]) []),
                                   (Just (renderArg d), Args (befores ++ [Dead]) [Live arg])
                                 ]
    it "tries to ignore this value that looks like an option value" $
      consumeArgument ["-p1", "--port", "2"]
        `shouldBe` [ (Nothing, Args ["-p1", "--port", "2"] []),
                     (Just "2", Args ["-p1", "--port", Dead] []),
                     (Just "--port", Args ["-p1", Dead] ["2"]),
                     (Just "-p1", Args [Dead] ["--port", "2"])
                   ]

  describe "consumeSwitch" $ do
    it "fails to consume if there are no dasheds" $
      forAllValid $ \as ->
        consumeSwitch [] as `shouldBe` Nothing
    it "fails to consume if there are no arguments" $
      forAllValid $ \ds ->
        consumeSwitch ds [] `shouldBe` Nothing

    it "does not consume a mismatched switch" $
      consumeSwitch ["--foo"] ["--bar"] `shouldBe` Nothing
    it "consumes a short switch if there are no other args" $
      consumeSwitch ["-v"] ["-v"] `shouldBe` Just [Dead]
    it "consumes a long switch if there are no other args" $
      consumeSwitch ["--verbose"] ["--verbose"] `shouldBe` Just [Dead]
    it "consumes a switch at the front first" $
      consumeSwitch ["-a", "-b"] ["-a", "-b"] `shouldBe` Just [Dead, "-b"]
    it "consumes a folded switch at the front first" $
      consumeSwitch ["-a", "-b"] ["-ab"] `shouldBe` Just ["-b"]

  describe "consumeOption" $ do
    it "fails to consume if there are no dasheds" $
      forAllValid $ \as ->
        consumeOption [] as `shouldBe` Nothing
    it "fails to consume if there are no arguments" $
      forAllValid $ \ds ->
        consumeOption ds [] `shouldBe` Nothing
    it "does not consume a mismatched option" $
      consumeOption ["--foo"] ["--bar", "quux"] `shouldBe` Nothing
    it "consumes a short option" $
      consumeOption ["-f"] ["-f", "foo"] `shouldBe` Just ("foo", [Dead])
    it "consumes a long option" $
      consumeOption ["--foo"] ["--foo", "foo"] `shouldBe` Just ("foo", [Dead])
    it "consumes an option at the front first" $
      consumeOption ["-f"] ["-f", "foo", "-f", "bar"] `shouldBe` Just ("foo", [Dead, "-f", "bar"])
    it "consumes a folded option" $
      consumeOption ["-f"] ["-vf", "foo"] `shouldBe` Just ("foo", ["-v", Dead])
    it "consumes a long option with an equals sign" $
      consumeOption ["--file"] ["--file=foo.txt"] `shouldBe` Just ("foo.txt", [Dead])
    it "consumes a short option in shorthand notation" $
      consumeOption ["-f"] ["-ffoo.txt"] `shouldBe` Just ("foo.txt", [Dead])
    it "consumes a short option before a long option" $
      consumeOption
        ["-p", "--port"]
        ["-p1", "--port", "2"]
        `shouldBe` Just ("1", [Dead, "--port", "2"])
    it "consumes a short option before a long option" $
      consumeOption
        ["-p", "--port"]
        [Dead, "--port", "2"]
        `shouldBe` Just ("2", [Dead, Dead])
