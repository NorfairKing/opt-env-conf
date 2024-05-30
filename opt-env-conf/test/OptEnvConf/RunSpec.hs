module OptEnvConf.RunSpec (spec) where

import Control.Applicative
import Data.Aeson as JSON (Object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.GenValidity.Aeson ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import OptEnvConf
import OptEnvConf.ArgMap (ArgMap (..), Dashed (..), Opt (..))
import qualified OptEnvConf.ArgMap as ArgMap
import OptEnvConf.ArgMap.Gen ()
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Error
import OptEnvConf.Parser
import OptEnvConf.Reader as Reader
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "unrecognisedOptions" $ do
    it "says that any argument is unrecognised when no arguments would be parsed" $
      forAllValid $ \args -> do
        let p = pure 'a'
        unrecognisedOptions p (ArgMap.parse_ args) `shouldBe` map OptArg args

    it "recognises arguments when they would be parsed" $
      forAllValid $ \arg -> do
        let p = strArgument [] :: Parser String
        let args = [arg]
        unrecognisedOptions p (ArgMap.parse_ args) `shouldBe` []

    it "says that an option is unrecognised when no options would not parsed" $
      forAllValid $ \d ->
        forAllValid $ \v -> do
          let p = pure 'a'
          let args = [ArgMap.renderDashed d, v]
          unrecognisedOptions p (ArgMap.parse_ args) `shouldBe` [OptOption d v]

    it "says that an option is unrecognised when that options would not parsed" $
      forAllValid $ \l1 -> do
        forAll (genValid `suchThat` (/= l1)) $ \l2 -> do
          forAllValid $ \v -> do
            let p = strOption [long (NE.toList l1)] :: Parser String
            let d = DashedLong l2
            let args = [ArgMap.renderDashed d, v]
            unrecognisedOptions p (ArgMap.parse_ args) `shouldBe` [OptOption d v]

    it "recognises an option that would be parsed" $
      forAllValid $ \l -> do
        forAllValid $ \v -> do
          let p = strOption [long $ NE.toList l] :: Parser String
          let args = [ArgMap.renderDashed (DashedLong l), v]
          unrecognisedOptions p (ArgMap.parse_ args) `shouldBe` []

  describe "runParser" $ do
    describe "Pure" $ do
      it "can parse a pure value from anything" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf ->
              forAllValid $ \expected ->
                shouldParse (pure expected) args env mConf (expected :: Int)

    describe "Fmap" $ do
      it "can parse a mapped value from anything" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let expected = succ i
                shouldParse (ParserFmap succ $ pure i) args env mConf (expected :: Int)

    describe "Ap" $ do
      it "can parse two values with ap" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = (,) <$> pure (succ i) <*> pure i
                let expected = (succ i, i :: Int)
                shouldParse p args env mConf expected

    describe "Empty" $ do
      it "can fail to parse an empty value" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf -> do
              let p = empty :: Parser Int
              shouldFail p args env mConf (ParseErrorEmpty :| [])

    describe "Alt" $ do
      it "can parse a Left value with Alt" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = (Left <$> pure i) <|> (Right <$> pure (succ i))
                let expected = Left (i :: Int)
                shouldParse p args env mConf expected

      it "can parse a Right value with Alt" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf ->
              forAllValid $ \i -> do
                let p = empty `ParserAlt` (Right <$> pure i)
                let expected = Right (i :: Int) :: Either Int Int
                shouldParse p args env mConf expected

    describe "Many" $ do
      it "can pass many args" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = ArgMap.empty {argMapOpts = map OptArg ls}
              let p = many $ strArgument []
              let expected = ls
              shouldParse p args env mConf expected

    describe "Some" $ do
      it "fails to parse zero args" $
        forAllValid $ \env ->
          forAllValid $ \mConf -> do
            let args = ArgMap.empty {argMapOpts = []}
            let ap = emptyArgumentParser
            let p = some $ ParserArg Reader.str ap :: Parser [String]
            shouldFail p args env mConf (ParseErrorMissingArgument (argumentOptDoc ap) :| [])

      it "can parse some args" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = ArgMap.empty {argMapOpts = map OptArg $ NE.toList ls}
              let p = some $ strArgument []
              let expected = NE.toList ls
              shouldParse p args env mConf expected

    describe "OptionalFirst" $ do
      it "parses nothing for an empty list of options" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf -> do
              let p = optionalFirst [] :: Parser (Maybe Int)
              shouldParse p args env mConf Nothing

      it "parses the first option if both are possible" $
        forAllValid $ \env' ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, arg) ->
              forAllValid $ \(var, val) -> do
                let args = ArgMap.empty {argMapOpts = [OptOption (DashedLong l) arg]}
                let env = EnvMap.insert var val env'
                let p =
                      optionalFirst
                        [ optional $
                            strOption
                              [ long $ NE.toList l
                              ],
                          optional $ envVar str var
                        ]
                shouldParse p args env mConf (Just arg)

    describe "RequiredFirst" $ do
      it "fails to parse for an empty list of options" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \mConf -> do
              let p = requiredFirst [] :: Parser (Maybe Int)
              shouldFail p args env mConf (ParseErrorRequired :| [])

      it "parses the first option if both are possible" $
        forAllValid $ \env' ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, arg) ->
              forAllValid $ \(var, val) -> do
                let args = ArgMap.empty {argMapOpts = [OptOption (DashedLong l) arg]}
                let env = EnvMap.insert var val env'
                let p =
                      requiredFirst
                        [ optional $
                            strOption
                              [ long $ NE.toList l
                              ],
                          optional $ envVar str var
                        ]
                shouldParse p args env mConf arg

    describe "Arg" $ do
      it "can parse a single arg" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \arg -> do
              let args = ArgMap.empty {argMapOpts = [OptArg arg]}
              let p = strArgument []
              let expected = arg
              shouldParse p args env mConf expected

    describe "Opt" $ do
      it "can parse a single option" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, r) -> do
              let args = ArgMap.empty {argMapOpts = [OptOption (DashedLong l) r]}
              let p = strOption [long $ NE.toList l]
              let expected = r
              shouldParse p args env mConf expected

      it "can parse a many of the same option" $
        forAllValid $ \env ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, rs) -> do
              let args = ArgMap.empty {argMapOpts = map (OptOption (DashedLong l)) rs}
              let p = many $ strOption [long $ NE.toList l]
              let expected = rs
              shouldParse p args env mConf expected

    describe "EnvVar" $ do
      it "can parse a single env var" $
        forAllValid $ \args ->
          forAllValid $ \env' ->
            forAllValid $ \mConf ->
              forAllValid $ \(key, val) -> do
                let env = EnvMap.insert key val env'
                let p = envVar str key
                let expected = val
                shouldParse p args env mConf expected

    describe "Config" $ do
      it "can parse a single config var" $
        forAllValid $ \args ->
          forAllValid $ \env ->
            forAllValid $ \conf ->
              forAllValid $ \(key, val) -> do
                let mConf = Just $ KeyMap.insert (Key.fromString key) val conf
                let p = confVal key
                let expected = val
                shouldParse p args env mConf expected

    describe "arguments" $ do
      argParseSpec
        ["--foo", "bar"]
        (strOption [long "foo"])
        "bar"
      argParseSpec
        ["--foo", "bar"]
        (many $ strOption [long "foo"])
        ["bar"]
      argParseSpec
        ["--foo", "bar", "--foo", "quux"]
        (many $ strOption [long "foo"])
        ["bar", "quux"]
      argParseSpec
        ["--foo", "bar", "-f", "quux"]
        (many $ strOption [short 'f', long "foo"])
        ["bar", "quux"]
      argParseSpec
        ["-f", "bar", "--foo", "quux"]
        (many $ strOption [short 'f', long "foo"])
        ["bar", "quux"]

argParseSpec :: (Show a, Eq a) => [String] -> Parser a -> a -> Spec
argParseSpec args p expected = do
  it (unwords ["parses ", show args, "as", show expected]) $ do
    let argMap = ArgMap.parse_ args
    errOrRes <- runParserOn p argMap EnvMap.empty Nothing
    case errOrRes of
      Left err -> expectationFailure $ show err
      Right actual -> actual `shouldBe` expected

shouldParse ::
  (Show a, Eq a) =>
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  a ->
  IO ()
shouldParse p args env mConf expected = do
  errOrRes <- runParserOn p args env mConf
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right actual -> actual `shouldBe` expected

shouldFail ::
  (Show a) =>
  Parser a ->
  ArgMap ->
  EnvMap ->
  Maybe JSON.Object ->
  NonEmpty ParseError ->
  IO ()
shouldFail p args env mConf expected = do
  errOrRes <- runParserOn p args env mConf
  case errOrRes of
    Left err -> err `shouldBe` expected
    Right actual -> expectationFailure $ show actual
