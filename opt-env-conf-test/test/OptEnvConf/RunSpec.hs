{-# LANGUAGE LambdaCase #-}

module OptEnvConf.RunSpec (spec) where

import Control.Applicative
import Data.Aeson as JSON (Object, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.GenValidity.Aeson ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..), Opt (..))
import qualified OptEnvConf.ArgMap as ArgMap
import OptEnvConf.ArgMap.Gen ()
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Error
import OptEnvConf.Parser
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
  describe "unrecognisedOptions" $ do
    it "says that any argument is unrecognised when no arguments would be parsed" $
      forAllValid $ \args -> do
        let p = pure 'a'
        unrecognisedOptions p (ArgMap.parse args) `shouldBe` map OptArg args

    it "recognises arguments when they would be parsed" $
      forAllValid $ \arg -> do
        let p = setting [reader str, argument] :: Parser String
        let args = [arg]
        unrecognisedOptions p (ArgMap.parse args) `shouldBe` []

    it "says that an option is unrecognised when no options would not parsed" $
      forAllValid $ \d ->
        forAllValid $ \v -> do
          let p = pure 'a'
          let args = [ArgMap.renderDashed d, v]
          unrecognisedOptions p (ArgMap.parse args) `shouldBe` [OptOption d v]

    it "says that an option is unrecognised when that options would not parsed" $
      forAllValid $ \l1 -> do
        forAll (genValid `suchThat` (/= l1)) $ \l2 -> do
          forAllValid $ \v -> do
            let p = setting [reader str, option, long (NE.toList l1)] :: Parser String
            let d = DashedLong l2
            let args = [ArgMap.renderDashed d, v]
            unrecognisedOptions p (ArgMap.parse args) `shouldBe` [OptOption d v]

    it "recognises an option that would be parsed" $
      forAllValid $ \l -> do
        forAllValid $ \v -> do
          let p = setting [reader str, option, long $ NE.toList l] :: Parser String
          let args = [ArgMap.renderDashed (DashedLong l), v]
          unrecognisedOptions p (ArgMap.parse args) `shouldBe` []

  describe "runParser" $ do
    describe "pure" $ do
      it "can parse a pure value from anything" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \expected ->
                shouldParse (pure expected) args e mConf (expected :: Int)

    describe "fmap" $ do
      it "can parse a mapped value from anything" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let expected = succ i
                shouldParse (fmap succ $ pure i) args e mConf (expected :: Int)

    describe "<*>" $ do
      it "can parse two values with ap" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = (,) <$> pure (succ i) <*> pure i
                let expected = (succ i, i :: Int)
                shouldParse p args e mConf expected

    describe "Select" $ do
      it "can use the second parser with select" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = select (pure (Left i :: Either Int Int)) (pure succ)
                let expected = succ i
                shouldParse p args e mConf expected

      it "can avoid the second parser with select" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = select (pure (Right i :: Either Int Int)) (pure succ)
                let expected = i
                shouldParse p args e mConf expected

    describe "Empty" $ do
      it "can fail to parse an empty value" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf -> do
              let p = empty :: Parser Int
              shouldFail p args e mConf $ \case
                ParseErrorEmpty :| [] -> True
                _ -> False

    describe "Alt" $ do
      it "can parse a Left value with Alt" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = (Left <$> pure i) <|> (Right <$> pure (succ i))
                let expected = Left (i :: Int)
                shouldParse p args e mConf expected

      it "can parse a Right value with Alt" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = empty `ParserAlt` (Right <$> pure i)
                let expected = Right (i :: Int) :: Either Int Int
                shouldParse p args e mConf expected

    describe "Many" $ do
      it "can pass many args" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = ArgMap.empty {argMapOpts = map OptArg ls}
              let p = many $ setting [reader str, argument]
              let expected = ls
              shouldParse p args e mConf expected

    describe "Some" $ do
      it "fails to parse zero args" $
        forAllValid $ \e ->
          forAllValid $ \mConf -> do
            let args = ArgMap.empty {argMapOpts = []}
            let p = some $ setting [reader str, argument] :: Parser [String]
            shouldFail p args e mConf $ \case
              ParseErrorMissingArgument _ :| [] -> True
              _ -> False

      it "can parse some args" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = ArgMap.empty {argMapOpts = map OptArg $ NE.toList ls}
              let p = some $ setting [reader str, argument]
              let expected = NE.toList ls
              shouldParse p args e mConf expected

    describe "MapIO" $ do
      it "can run an IO action on the result of a parser" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = mapIO (pure . succ) (pure (i :: Int))
                let expected = succ i
                shouldParse p args e mConf expected

    describe "WithConfig" $ do
      it "can replace the config object" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \c1 ->
              forAllValid $ \c2' ->
                forAllValid $ \(key, val) -> do
                  let c2 = KeyMap.insert key (toJSON val) c2'
                  let p =
                        withConfig (pure (Just c2)) $
                          setting [conf (Key.toString key)]
                  let expected = val :: Text
                  shouldParse p args e (Just c1) expected

    describe "subArgs" $ do
      it "can prefix a switch parser" $
        forAllValid $ \a' ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \prefix ->
                forAllValid $ \(key, val) -> do
                  let prefixedKey = ArgMap.prefixDashed prefix (DashedLong key)
                  let a = a' {argMapOpts = OptSwitch prefixedKey : argMapOpts a'}
                  let p =
                        subArgs prefix $
                          setting
                            [ reader str,
                              switch val,
                              long (NE.toList key)
                            ]
                  let expected = val :: String
                  shouldParse p a e mConf expected

      it "can prefix an option parser" $
        forAllValid $ \a' ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \prefix ->
                forAllValid $ \(key, val) -> do
                  let prefixedKey = ArgMap.prefixDashed prefix (DashedLong key)
                  let a = a' {argMapOpts = OptOption prefixedKey val : argMapOpts a'}
                  let p =
                        subArgs prefix $
                          setting
                            [ reader str,
                              option,
                              long (NE.toList key)
                            ]
                  let expected = val
                  shouldParse p a e mConf expected

    describe "subEnv" $ do
      it "can prefix an env var parser" $
        forAllValid $ \args ->
          forAllValid $ \e' ->
            forAllValid $ \mConf ->
              forAllValid $ \prefix ->
                forAllValid $ \(key, val) -> do
                  let prefixedKey = prefix <> key
                  let e = EnvMap.insert prefixedKey val e'
                  let p = subEnv prefix $ setting [reader str, env key]
                  let expected = val
                  shouldParse p args e mConf expected

    describe "subConfig" $ do
      it "can prefix a conf val parser" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \c' ->
              forAllValid $ \prefix ->
                forAllValid $ \(key, val) -> do
                  let c = KeyMap.insert prefix (toJSON (KeyMap.singleton key (toJSON val))) c'
                  let p =
                        subConfig (Key.toString prefix) $
                          setting [conf (Key.toString key)]
                  let expected = val :: Text
                  shouldParse p args e (Just c) expected

    describe "Setting" $ do
      it "can parse a single arg" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \arg -> do
              let args = ArgMap.empty {argMapOpts = [OptArg arg]}
              let p = setting [reader str, argument]
              let expected = arg
              shouldParse p args e mConf expected

      it "can parse a single option" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, r) -> do
              let args = ArgMap.empty {argMapOpts = [OptOption (DashedLong l) r]}
              let p = setting [reader str, option, long $ NE.toList l]
              let expected = r
              shouldParse p args e mConf expected

      it "can parse a many of the same option" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, rs) -> do
              let args = ArgMap.empty {argMapOpts = map (OptOption (DashedLong l)) rs}
              let p = many $ setting [reader str, option, long $ NE.toList l]
              let expected = rs
              shouldParse p args e mConf expected

      it "can parse a single env var" $
        forAllValid $ \args ->
          forAllValid $ \e' ->
            forAllValid $ \mConf ->
              forAllValid $ \(key, val) -> do
                let e = EnvMap.insert key val e'
                let p = setting [reader str, env key]
                let expected = val
                shouldParse p args e mConf expected

      it "can parse a single config value" $
        forAllValid $ \args ->
          forAllValid $ \e ->
            forAllValid $ \c' ->
              forAllValid $ \(key, val) -> do
                let c = KeyMap.insert key (toJSON val) c'
                let p = setting [conf (Key.toString key)]
                let expected = val :: Text
                shouldParse p args e (Just c) expected

    describe "Unit tests" $ do
      argParseSpec
        ["--foo", "bar"]
        (setting [reader str, option, long "foo"])
        "bar"
      argParseSpec
        ["--foo", "bar"]
        (many $ setting [reader str, option, long "foo"])
        ["bar"]
      argParseSpec
        ["--foo", "bar", "--foo", "quux"]
        (many $ setting [reader str, option, long "foo"])
        ["bar", "quux"]
      argParseSpec
        ["--foo", "bar", "-f", "quux"]
        (many $ setting [reader str, option, short 'f', long "foo"])
        ["bar", "quux"]
      argParseSpec
        ["-f", "bar", "--foo", "quux"]
        (many $ setting [reader str, option, short 'f', long "foo"])
        ["bar", "quux"]

      argParseSpecs
        (enableDisableSwitch True [long "example", env "EXAMPLE", conf "example"])
        [ ([], True),
          (["--enable-example"], True),
          (["--disable-example"], False)
        ]

      argParseSpecs
        (enableDisableSwitch False [long "example", env "EXAMPLE", conf "example"])
        [ ([], False),
          (["--enable-example"], True),
          (["--disable-example"], False)
        ]

      envParseSpecs
        (enableDisableSwitch True [long "example", env "EXAMPLE", env "ALTERNATIVE", conf "example"])
        [ ([], True),
          ([("EXAMPLE", "False")], False),
          ([("ALTERNATIVE", "False")], False),
          ([("EXAMPLE", "True")], True),
          ([("ALTERNATIVE", "True")], True)
        ]

      envParseSpecs
        (enableDisableSwitch False [long "example", env "EXAMPLE", env "ALTERNATIVE", conf "example"])
        [ ([], False),
          ([("EXAMPLE", "True")], True),
          ([("ALTERNATIVE", "True")], True),
          ([("EXAMPLE", "False")], False),
          ([("ALTERNATIVE", "False")], False)
        ]

      argParseSpecs
        ( commands
            [ command "one" "first" $ pure '1',
              command "two" "second" $ pure '2'
            ]
        )
        [ (["one"], '1'),
          (["two"], '2')
        ]

argParseSpecs :: (HasCallStack) => (Show a, Eq a) => Parser a -> [([String], a)] -> Spec
argParseSpecs p table = withFrozenCallStack $ mapM_ (\(args, result) -> argParseSpec args p result) table

argParseSpec :: (HasCallStack) => (Show a, Eq a) => [String] -> Parser a -> a -> Spec
argParseSpec args p expected = withFrozenCallStack $ do
  it (unwords ["parses ", show args, "as", show expected]) $ do
    let argMap = ArgMap.parse args
    errOrRes <- runParserOn p argMap EnvMap.empty Nothing
    case errOrRes of
      Left err -> expectationFailure $ show err
      Right actual -> actual `shouldBe` expected

envParseSpecs :: (HasCallStack) => (Show a, Eq a) => Parser a -> [([(String, String)], a)] -> Spec
envParseSpecs p table = withFrozenCallStack $ mapM_ (\(envs, result) -> envParseSpec envs p result) table

envParseSpec :: (HasCallStack) => (Show a, Eq a) => [(String, String)] -> Parser a -> a -> Spec
envParseSpec envVars p expected = withFrozenCallStack $ do
  it (unwords ["parses ", show envVars, "as", show expected]) $ do
    let envMap = EnvMap.parse envVars
    errOrRes <- runParserOn p ArgMap.empty envMap Nothing
    case errOrRes of
      Left err -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors err
      Right actual -> actual `shouldBe` expected

shouldParse ::
  (Show a, Eq a) =>
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  a ->
  IO ()
shouldParse p args e mConf expected = do
  errOrRes <- runParserOn p args e mConf
  context (showParserABit p) $ case errOrRes of
    Left errs -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors errs
    Right actual -> actual `shouldBe` expected

shouldFail ::
  (Show a) =>
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  (NonEmpty ParseError -> Bool) ->
  IO ()
shouldFail p args e mConf isExpected = do
  errOrRes <- runParserOn p args e mConf
  case errOrRes of
    Left errs -> errs `shouldSatisfy` isExpected
    Right actual -> expectationFailure $ show actual
