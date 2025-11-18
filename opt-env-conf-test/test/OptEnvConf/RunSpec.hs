{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OptEnvConf.RunSpec (spec) where

import Autodocodec
import Control.Applicative
import Control.Concurrent
import Data.Aeson as JSON (Object, Value (Null), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.GenValidity.Aeson ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.Args as Args
import OptEnvConf.Args.Gen ()
import OptEnvConf.EnvMap (EnvMap (..))
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.EnvMap.Gen ()
import OptEnvConf.Error
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
  describe "runParser" $ do
    describe "pure" $ do
      it "can parse a pure value from anything" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \expected ->
              shouldParse (pure expected) Args.emptyArgs e mConf (expected :: Int)

    describe "fmap" $ do
      it "can parse a mapped value from anything" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let expected = succ i
              shouldParse (fmap succ $ pure i) Args.emptyArgs e mConf (expected :: Int)

    describe "<*>" $ do
      it "can parse two values with ap" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = (,) <$> pure (succ i) <*> pure i
              let expected = (succ i, i :: Int)
              shouldParse p Args.emptyArgs e mConf expected

    describe "Select" $ do
      it "can use the second parser with select" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = select (pure (Left i :: Either Int Int)) (pure succ)
              let expected = succ i
              shouldParse p Args.emptyArgs e mConf expected

      it "can avoid the second parser with select" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = select (pure (Right i :: Either Int Int)) (pure succ)
              let expected = i
              shouldParse p Args.emptyArgs e mConf expected

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
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = (Left <$> pure i) <|> (Right <$> pure (succ i))
              let expected = Left (i :: Int)
              shouldParse p Args.emptyArgs e mConf expected

      it "can parse a Right value with Alt" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = empty `ParserAlt` (Right <$> pure i)
              let expected = Right (i :: Int) :: Either Int Int
              shouldParse p Args.emptyArgs e mConf expected

    describe "Many" $ do
      it "can pass many args" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = parseArgs ls
              let p = many $ setting [reader str, argument]
              let expected = ls
              shouldParse p args e mConf expected

    describe "Some" $ do
      it "fails to parse zero args" $
        forAllValid $ \e ->
          forAllValid $ \mConf -> do
            let args = emptyArgs
            let p = some $ setting [reader str, argument] :: Parser [String]
            shouldFail p args e mConf $ \case
              ParseErrorMissingArgument _ :| [] -> True
              _ -> False

      it "can parse some args" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \ls -> do
              let args = parseArgs (NE.toList ls)
              let p = some $ setting [reader str, argument]
              let expected = NE.toList ls
              shouldParse p args e mConf expected

    describe "MapIO" $ do
      it "can run an IO action on the result of a parser" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \i -> do
              let p = mapIO (pure . succ) (pure (i :: Int))
              let expected = succ i
              shouldParse p Args.emptyArgs e mConf expected

    describe "RequireCapability" $ do
      it "can run the check parser if the capability is available" $
        forAllValid $ \capabilitiesPrototype ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \result ->
                forAllValid $ \capName -> do
                  let p :: Parser Int
                      p = requireCapability capName $ checkMapEither (const (Left "failed")) (pure (result :: Int))
                  let cap = Capability (T.pack capName)
                  let capabilities = enableCapability cap capabilitiesPrototype
                  shouldFail' p capabilities Args.emptyArgs e mConf $ \case
                    ParseErrorCheckFailed False "failed" :| [] -> True
                    _ -> False

      it "cannot run the check parser if the capability is not available" $
        forAllValid $ \capabilitiesPrototype ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \result ->
                forAllValid $ \capName -> do
                  let p :: Parser Int
                      p = requireCapability capName $ checkMapEither (const (Left "failed")) (pure (result :: Int))
                  let cap = Capability (T.pack capName)
                  let capabilities = disableCapability cap capabilitiesPrototype
                  shouldFail' p capabilities Args.emptyArgs e mConf $ \case
                    ParseErrorMissingCapability _ :| [] -> True
                    _ -> False

      it "still runs the below parser when a capability is missing for the check parser" $
        forAllValid $ \capabilitiesPrototype ->
          forAllValid $ \capName ->
            forAllValid $ \result ->
              forAllValid $ \e ->
                forAllValid $ \mConf -> do
                  var <- newMVar 0
                  let p = requireCapability capName (mapIO pure (mapIO (swapMVar var) (pure (result :: Int))))
                  let cap = Capability (T.pack capName)
                  let capabilities = disableCapability cap capabilitiesPrototype
                  errOrRes <- runParserOn capabilities Nothing p Args.emptyArgs e mConf
                  case errOrRes of
                    Left errs -> do
                      NE.map parseErrorMessage errs
                        `shouldSatisfy` ( \case
                                            ParseErrorMissingCapability c :| [] | c == cap -> True
                                            _ -> False
                                        )
                      readMVar var `shouldReturn` result -- instead of 1
                    Right _ -> expectationFailure "The parser should not have succeeded."

      it "can run the setting parser if the capability is available" $
        forAllValid $ \capabilitiesPrototype ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \capName -> do
                let p :: Parser Int
                    p = setting [argument, reader auto, requiredCapability capName] :: Parser Int
                let cap = Capability (T.pack capName)
                let capabilities = enableCapability cap capabilitiesPrototype
                shouldFail' p capabilities Args.emptyArgs e mConf $ \case
                  ParseErrorMissingArgument _ :| [] -> True
                  _ -> False

      it "cannot run the setting parser if the capability is available" $
        forAllValid $ \capabilitiesPrototype ->
          forAllValid $ \e ->
            forAllValid $ \mConf ->
              forAllValid $ \capName -> do
                let p :: Parser Int
                    p = setting [argument, reader auto, requiredCapability capName] :: Parser Int
                let cap = Capability (T.pack capName)
                let capabilities = disableCapability cap capabilitiesPrototype
                shouldFail' p capabilities Args.emptyArgs e mConf $ \case
                  ParseErrorMissingCapability _ :| [] -> True
                  _ -> False

    describe "WithConfig" $ do
      it "can replace the config object" $
        forAllValid $ \e ->
          forAllValid $ \c1 ->
            forAllValid $ \c2' ->
              forAllValid $ \(key, val) -> do
                let c2 = KeyMap.insert key (toJSON val) c2'
                let p =
                      withConfig (pure (Just c2)) $
                        setting [conf (Key.toString key)]
                let expected = val :: Text
                shouldParse p Args.emptyArgs e (Just c1) expected

    describe "withFirstYamlConfig" $ do
      it "can parse without any arguments" $
        forAllValid $ \e ->
          forAllValid $ \val -> do
            let p = withFirstYamlConfig (pure []) (pure val) :: Parser String
            shouldParse p Args.emptyArgs e Nothing val

    describe "withCombinedYamlConfig" $ do
      it "can parse without any arguments" $ do
        forAllValid $ \e ->
          forAllValid $ \val -> do
            let p = withCombinedYamlConfigs (pure []) (pure val) :: Parser String
            shouldParse p Args.emptyArgs e Nothing val

    describe "subArgs" $ do
      it "can prefix a switch parser" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \prefix ->
              forAllValid $ \(key, val) -> do
                let prefixedKey = Args.prefixDashed prefix (DashedLong key)
                let a = Args.parseArgs [Args.renderDashed prefixedKey]
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
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \prefix ->
              forAllValid $ \(key, val) -> do
                let prefixedKey = Args.prefixDashed prefix (DashedLong key)
                let a = parseArgs [renderDashed prefixedKey, val]
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
        forAllValid $ \e' ->
          forAllValid $ \mConf ->
            forAllValid $ \prefix ->
              forAllValid $ \(key, val) -> do
                let prefixedKey = prefix <> key
                let e = EnvMap.insert prefixedKey val e'
                let p = subEnv prefix $ setting [reader str, env key]
                let expected = val
                shouldParse p Args.emptyArgs e mConf expected

    describe "subConfig" $ do
      it "can prefix a conf val parser" $
        forAllValid $ \e ->
          forAllValid $ \c' ->
            forAllValid $ \prefix ->
              forAllValid $ \(key, val) -> do
                let c = KeyMap.insert prefix (toJSON (KeyMap.singleton key (toJSON val))) c'
                let p =
                      subConfig (Key.toString prefix) $
                        setting [conf (Key.toString key)]
                let expected = val :: Text
                shouldParse p Args.emptyArgs e (Just c) expected

    describe "Setting" $ do
      it "can parse a single arg" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \arg -> do
              let args = Args.parseArgs [arg]
              let p = setting [reader str, argument]
              let expected = arg
              shouldParse p args e mConf expected

      it "can parse a single option" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, r) -> do
              let args =
                    parseArgs
                      [ Args.renderDashed (DashedLong l),
                        r
                      ]
              let p = setting [reader str, option, long $ NE.toList l]
              let expected = r
              shouldParse p args e mConf expected

      it "can parse a many of the same option" $
        forAllValid $ \e ->
          forAllValid $ \mConf ->
            forAllValid $ \(l, rs) -> do
              let args = parseArgs $ concatMap (\v -> [Args.renderDashed (DashedLong l), v]) rs
              let p = many $ setting [reader str, option, long $ NE.toList l]
              let expected = rs :: [String]
              shouldParse p args e mConf expected

      it "can parse a single env var" $
        forAllValid $ \e' ->
          forAllValid $ \mConf ->
            forAllValid $ \(key, val) -> do
              let e = EnvMap.insert key val e'
              let p = setting [reader str, env key]
              let expected = val
              shouldParse p Args.emptyArgs e mConf expected

      it "can parse a single config value" $
        forAllValid $ \e ->
          forAllValid $ \c' ->
            forAllValid $ \(key, val) -> do
              let c = KeyMap.insert key (toJSON val) c'
              let p = setting [conf (Key.toString key)]
              let expected = val :: Text
              shouldParse p Args.emptyArgs e (Just c) expected

      it "parses Null as the default value" $
        forAllValid $ \e ->
          forAllValid $ \c' ->
            forAllValid $ \key -> do
              let c = KeyMap.insert key JSON.Null c'
              let defaultVal = "hi" :: String
              let p = setting [conf (Key.toString key), value defaultVal]
              let expected = defaultVal
              shouldParse p Args.emptyArgs e (Just c) expected

    describe "Unit tests" $ do
      argParseSpec
        ["--foo", "bar"]
        (setting [reader str, option, long "foo"])
        ("bar" :: String)
      argParseSpec
        ["--foo", "bar"]
        (many $ setting [reader str, option, long "foo"])
        (["bar"] :: [String])
      argParseSpec
        ["--foo", "bar", "--foo", "quux"]
        (many $ setting [reader str, option, long "foo"])
        (["bar", "quux"] :: [String])
      argParseSpec
        ["--foo", "bar", "-f", "quux"]
        (many $ setting [reader str, option, short 'f', long "foo"])
        (["bar", "quux"] :: [String])
      argParseSpec
        ["-f", "bar", "--foo", "quux"]
        (many $ setting [reader str, option, short 'f', long "foo"])
        (["bar", "quux"] :: [String])

      argParseSpec
        ["--", "all", "-bare", "--arguments"]
        (many $ setting [reader str, argument])
        -- No double-dash!
        (["all", "-bare", "--arguments"] :: [String])

      -- Switch before argument
      argParseSpec
        ["--foo", "bar"]
        ((,) <$> setting [switch True, long "foo"] <*> setting [reader str, argument])
        (True, "bar" :: String)

      -- Note that this could also be parsed as (False, "--foo") with a
      -- leftover argument but somehow that's not what we want.
      -- (Maybe we could "just" disallow extra arguments while parsing?
      -- No because then "--help" doesn't work.)
      argParseSpec
        ["--foo", "bar"]
        ((,) <$> setting [switch True, long "foo", value False] <*> setting [reader str, argument])
        (True, "bar" :: String)

      -- Switch before argument, but defined after
      argParseSpec
        ["--foo", "bar"]
        ((,) <$> setting [reader str, argument] <*> setting [switch True, long "foo"])
        ("bar" :: String, True)

      -- Dashed as argument
      -- This example shows that we can't just skip dasheds when looking for
      -- arguments.
      argParseSpec
        ["--foo", "bar"]
        ((,) <$> setting [reader str, argument] <*> setting [reader str, argument])
        ("--foo" :: String, "bar" :: String)

      -- Dashed as value for an option
      -- This example shows that we can't "just" treat any argument with dashed
      -- as an option key or switch.
      -- They could be a value.
      argParseSpec
        ["--foo", "--bar"]
        (setting [reader str, option, long "foo"])
        ("--bar" :: String)

      -- Short dashed as a value for an option
      -- This example shows that we can't "just" unfold combined short options.
      -- They could be a value.
      argParseSpec
        ["--foo", "-dfu"]
        (setting [reader str, option, long "foo"])
        ("-dfu" :: String)

      -- Arguments can be parsed as-is
      argParseSpec
        ["delete"]
        ( commands
            [ command "add" "add" (pure "hi"),
              command "delete" "delete" (pure "ho")
            ]
        )
        ("ho" :: String)

      -- A command can have an argument
      argParseSpec
        ["add", "hi"]
        ( commands
            [ command "add" "add" $ setting [argument, reader str],
              command "other" "other" (pure "ho")
            ]
        )
        ("hi" :: String)

      -- Here an argument has a value that looks like a command name but
      -- should still be treated as an argument.
      argParseSpec
        ["command", "arg"]
        ( (,)
            <$> setting [reader str, argument]
            <*> commands
              [ command "command" "command" (pure '1'),
                command "arg" "command" (pure '2')
              ]
        )
        ("command" :: String, '2')

      -- Same as above but with an option instead of an argument
      argParseSpec
        ["--option", "command", "arg"]
        ( (,)
            <$> setting [reader str, option, long "option"]
            <*> commands
              [ command "command" "command" (pure '1'),
                command "arg" "command" (pure '2')
              ]
        )
        ("command" :: String, '2')

      -- Commands can have a default
      argParseSpec
        []
        ( commands
            [ command "a" "a" (pure "a"),
              command "b" "b" (pure "b"),
              defaultCommand "b"
            ]
        )
        ("b" :: String)

      -- Commands can have a default with an argument
      argParseSpec
        ["c"]
        ( commands
            [ command "a" "a" (pure "a"),
              command "b" "b" $ setting [argument, reader str],
              defaultCommand "b"
            ]
        )
        ("c" :: String)

      -- Unfolding switches
      argParseSpecs
        ( length
            <$> many
              ( setting
                  [ help "verbosity",
                    switch (),
                    long "verbose",
                    short 'v'
                  ]
              )
        )
        [ ([], 0),
          (["-v"], 1),
          (["--verbose"], 1),
          (["-v", "-v"], 2),
          (["-v", "--verbose"], 2),
          (["-vv"], 2),
          (["-vv", "--verbose"], 3),
          (["-vv", "--verbose", "-v"], 4)
        ]

      -- Unfolding short options as well
      argParseSpecs
        ( (,)
            <$> ( length
                    <$> many
                      ( setting
                          [ help "verbosity",
                            switch (),
                            short 'v'
                          ]
                      )
                )
            <*> setting
              [ reader str,
                option,
                short 'f',
                example ("file" :: String)
              ]
        )
        [ (["-f", "foo"], (0, "foo" :: String)),
          (["-v", "-f", "foo"], (1, "foo")),
          (["-vf", "foo", "-v"], (2, "foo")),
          (["-vvf", "foo"], (2, "foo")),
          (["-vvf", "foo", "-v"], (3, "foo"))
        ]

      -- Unfolding short switches
      argParseSpecs
        ( (,)
            <$> many
              ( setting
                  [ reader str,
                    option,
                    long "file",
                    short 'f'
                  ]
              )
            <*> ( length
                    <$> many
                      ( setting
                          [ help "verbosity",
                            switch (),
                            short 'v'
                          ]
                      )
                )
        )
        [ ([], ([], 0)),
          (["--file", "foo", "-v"], (["foo" :: String], 1)),
          (["-vf", "foo"], (["foo"], 1))
        ]
      -- Equals version of long option
      argParseSpecs
        ( setting
            [ option,
              reader str,
              long "file"
            ]
        )
        [ (["--file", "foo"], "foo" :: String),
          (["--file=foo"], "foo")
        ]

      -- Shorthand version of short option
      argParseSpecs
        ( setting
            [ option,
              reader str,
              short 'f'
            ]
        )
        [ (["-f", "foo"], "foo" :: String),
          (["-ffoo"], "foo")
        ]

      -- Optional Argument and optional switch
      let optionalArgOptionalSwitchExamples =
            [ (["foo", "-w"], (Just "foo", True)),
              (["-w", "foo"], (Just "foo", True)),
              (["foo", "--watch"], (Just "foo", True)),
              (["--watch", "foo"], (Just "foo", True)),
              (["foo"], (Just "foo", False)),
              (["--bar"], (Just "--bar", False)),
              (["-w"], (Nothing, True)),
              (["--watch"], (Nothing, True))
            ]
      argParseSpecs
        ( (,)
            <$> optional (setting [reader str, argument])
            <*> setting [switch True, short 'w', long "watch", value False] ::
            Parser (Maybe String, Bool)
        )
        optionalArgOptionalSwitchExamples
      argParseSpecs
        ( swap
            <$> ( (,)
                    <$> setting [switch True, short 'w', long "watch", value False]
                    <*> optional (setting [reader str, argument])
                ) ::
            Parser (Maybe String, Bool)
        )
        optionalArgOptionalSwitchExamples

      let verbosityArgsExamples =
            [ ([], ([], 0)),
              (["-vvv"], ([], 3)),
              (["--"], (["--"], 0)),
              (["-v", "--"], (["--"], 1)),
              (["-vv", "--"], (["--"], 2)),
              (["--", "-v"], (["-v"], 0)),
              (["a", "-v", "b", "-v"], (["a", "b"], 2)),
              (["a", "--verbose", "-v", "b", "-v"], (["a", "b"], 3)),
              (["a", "--verbose", "-v", "-v", "b", "-v"], (["a", "b"], 4)),
              (["a", "--verbose", "-v", "-v", "b", "-v", "c"], (["a", "b", "c"], 4)),
              (["a", "--verbose", "-v", "-vv", "b", "-v", "c"], (["a", "b", "c"], 5)),
              (["a", "--verbose", "-v", "-vv", "-v", "b", "-v", "c"], (["a", "b", "c"], 6))
            ]
      argParseSpecs
        ( (,)
            <$> many (setting [reader str, argument])
            <*> (length <$> many (setting [switch (), short 'v', long "verbose"])) ::
            Parser ([String], Int)
        )
        verbosityArgsExamples
      argParseSpecs
        ( swap
            <$> ( (,)
                    <$> (length <$> many (setting [switch (), short 'v', long "verbose"]))
                    <*> many (setting [reader str, argument])
                ) ::
            Parser ([String], Int)
        )
        verbosityArgsExamples

      let optionsArgsExamples =
            [ ([], ([], [])),
              (["-p1"], ([], [1])),
              (["-p1", "--port", "2"], ([], [1, 2])),
              (["-p", "1", "-p", "2", "-p", "3"], ([], [1, 2, 3])),
              (["--port=2"], ([], [2])),
              (["--port=2", "-p3"], ([], [2, 3])),
              (["--port", "3"], ([], [3])),
              (["foo", "-p4"], (["foo"], [4])),
              (["bar", "--port=5"], (["bar"], [5])),
              (["quux", "--port", "6"], (["quux"], [6])),
              (["-p7", "foo"], (["foo"], [7])),
              (["--port=8", "bar"], (["bar"], [8])),
              (["--port", "9", "quux"], (["quux"], [9])),
              (["--", "-p1"], (["-p1"], [])),
              (["--", "-p", "2"], (["-p", "2"], [])),
              (["--", "--port=3"], (["--port=3"], [])),
              (["--", "--port", "4"], (["--port", "4"], []))
            ]
      argParseSpecs
        ( (,)
            <$> many (setting [reader str, argument])
            <*> many (setting [reader auto, option, short 'p', long "port"]) ::
            Parser ([String], [Int])
        )
        optionsArgsExamples
      argParseSpecs
        ( swap
            <$> ( (,)
                    <$> many (setting [reader auto, option, short 'p', long "port"])
                    <*> many (setting [reader str, argument])
                ) ::
            Parser ([String], [Int])
        )
        optionsArgsExamples

      argParseSpecs
        (enableDisableSwitch [long "example", env "EXAMPLE", conf "example", value True])
        [ ([], True),
          (["--enable-example"], True),
          (["--disable-example"], False)
        ]

      argParseSpecs
        (enableDisableSwitch [long "example", env "EXAMPLE", conf "example", value False])
        [ ([], False),
          (["--enable-example"], True),
          (["--disable-example"], False)
        ]

      argParseSpecs
        (optional $ enableDisableSwitch [long "example", env "EXAMPLE", conf "example"])
        [ ([], Nothing),
          (["--enable-example"], Just True),
          (["--disable-example"], Just False)
        ]

      envParseSpecs
        (enableDisableSwitch [long "example", env "EXAMPLE", env "ALTERNATIVE", conf "example", value True])
        [ ([], True),
          ([("EXAMPLE", "False")], False),
          ([("ALTERNATIVE", "False")], False),
          ([("EXAMPLE", "True")], True),
          ([("ALTERNATIVE", "True")], True)
        ]

      envParseSpecs
        (enableDisableSwitch [long "example", env "EXAMPLE", env "ALTERNATIVE", conf "example", value False])
        [ ([], False),
          ([("EXAMPLE", "True")], True),
          ([("ALTERNATIVE", "True")], True),
          ([("EXAMPLE", "False")], False),
          ([("ALTERNATIVE", "False")], False)
        ]

      argParseSpecs
        (yesNoSwitch [long "example", env "EXAMPLE", conf "example", value True])
        [ ([], True),
          (["--example"], True),
          (["--no-example"], False)
        ]

      argParseSpecs
        (yesNoSwitch [long "example", env "EXAMPLE", conf "example", value False])
        [ ([], False),
          (["--example"], True),
          (["--no-example"], False)
        ]

      argParseSpecs
        (optional $ yesNoSwitch [long "example", env "EXAMPLE", conf "example"])
        [ ([], Nothing),
          (["--example"], Just True),
          (["--no-example"], Just False)
        ]

      envParseSpecs
        (yesNoSwitch [long "example", env "EXAMPLE", env "ALTERNATIVE", conf "example", value True])
        [ ([], True),
          ([("EXAMPLE", "False")], False),
          ([("ALTERNATIVE", "False")], False),
          ([("EXAMPLE", "True")], True),
          ([("ALTERNATIVE", "True")], True)
        ]

      envParseSpecs
        (yesNoSwitch [long "example", env "EXAMPLE", env "ALTERNATIVE", conf "example", value False])
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

      argParseSpecs
        ((Left <$> setting [switch True, long "bash-completion-script"]) <|> (Right <$> setting [reader str, argument]))
        [ (["--bash-completion-script"], Left True),
          (["arg"], Right ("arg" :: String))
        ]
      argParseSpecs
        ((Left <$> setting [switch True, long "bash-completion-script"]) <|> (Right <$> optional (setting [reader str, argument])))
        [ (["--bash-completion-script"], Left True),
          (["arg"], Right (Just ("arg" :: String)))
        ]

      argParseSpecs
        ( optional
            ( allOrNothing
                ( (,)
                    <$> setting [reader str, option, long "foo"]
                    <*> optional (setting [reader str, option, long "bar"])
                )
            ) ::
            Parser (Maybe (String, Maybe String))
        )
        [ ([], Nothing),
          (["--foo", "foo"], Just ("foo", Nothing))
        ]

      -- Default values should not count as parsed:
      argParseSpecs
        ( choice
            [ allOrNothing $
                (,)
                  <$> setting [option, long "foo", reader auto, help "This one will exist", metavar "CHAR", value 'c']
                  <*> setting [option, long "bar", reader auto, help "This one will not exist", metavar "CHAR"],
              pure ('a', 'b')
            ]
        )
        [ ([], ('a', 'b')),
          (["--foo", "'c'", "--bar", "'d'"], ('c', 'd'))
        ]

      argParseSpecs
        ( (,)
            <$> setting [option, long "before", reader auto, help "before", metavar "CHAR"]
            <*> optional
              ( allOrNothing $
                  (,)
                    <$> setting [option, long "baz", reader auto, help "This one will exist", metavar "CHAR"]
                    <*> setting [option, long "quux", reader auto, help "This one will not exist", metavar "CHAR"]
              ) ::
            Parser (Char, Maybe (Char, Char))
        )
        [ (["--before", "'m'"], ('m', Nothing))
        ]

      -- source location does not matter
      argParseSpecs
        ( optional
            ( let foo = setting [reader str, option, long "foo"]
               in (,)
                    <$> foo
                    <*> optional
                      ( subAll
                          "bar"
                          ( allOrNothing
                              ( (,)
                                  <$> foo
                                  <*> setting [reader str, option, long "bar"]
                              )
                          )
                      )
            ) ::
            Parser (Maybe (String, Maybe (String, String)))
        )
        [ ([], Nothing),
          (["--foo", "foo"], Just ("foo", Nothing))
        ]

      -- Failing after consuming args leaves the args unconsumed
      argParseSpecs
        ( choice
            [ (++)
                <$> many (setting [reader str, argument])
                <*> setting [switch ["ho"], long "--switch"],
              many $ setting [reader str, argument]
            ]
        )
        [ (["args", "here"], ["args", "here" :: String])
        ]

      -- Unrecognised args should be forgivable
      argParseSpecs
        ( let fallback = setting [reader str, option, long "foo"]
              withCommand = commands [command "run" "command in front" fallback]
           in withCommand <|> fallback
        )
        [ (["run", "--foo", "bar"], "bar" :: String),
          (["--foo", "bar"], "bar" :: String)
        ]

      -- Alt-ing commands concats the commands
      argParseSpecs
        ( let fallback = setting [reader str, option, long "foo"]
              withCommand1 = commands [command "run1" "command in front" fallback]
              withCommand2 = commands [command "run2" "command in front" fallback]
           in withCommand1 <|> withCommand2 <|> fallback
        )
        [ (["run1", "--foo", "bar"], "bar" :: String),
          (["run2", "--foo", "bar"], "bar" :: String),
          (["--foo", "bar"], "bar" :: String)
        ]

      -- Alt-ing commands concats the commands, even recursively
      argParseSpecs
        ( let fallback = setting [reader str, option, long "foo"]
              withCommand1 = commands [command "run1" "command in front" fallback]
              withCommand2 = commands [command "run2" "command in front" fallback]
           in withCommand1 <|> (withCommand2 <|> fallback)
        )
        [ (["run1", "--foo", "bar"], "bar" :: String),
          (["run2", "--foo", "bar"], "bar" :: String),
          (["--foo", "bar"], "bar" :: String)
        ]

      -- Required config value
      confParseSpecs
        (setting [conf "hi"] :: Parser String)
        [ (Just [("hi", "ho")], "ho")
        ]

      -- Optional config value
      confParseSpecs
        (optional $ setting [conf "hi"] :: Parser (Maybe String))
        [ (Nothing, Nothing),
          (Just [("hi", "ho")], Just "ho")
        ]

      -- Multiple possible codecs
      confParseSpecs
        ( setting
            [ confWith "hi" (Left <$> codec),
              confWith "foo" (Right <$> codec)
            ]
        )
        [ (Just [("hi", toJSON (5 :: Int))], Left 5),
          (Just [("foo", "bar")], Right "bar" :: Either Int String)
        ]

      argParseSpecs
        ( withDefault
            False
            ( setting
                [ argument,
                  reader auto
                ]
            )
        )
        [ (["True"], True),
          ([], False)
        ]

argParseSpecs :: (HasCallStack) => (Show a, Eq a) => Parser a -> [([String], a)] -> Spec
argParseSpecs p table = withFrozenCallStack $ mapM_ (\(args, result) -> argParseSpec args p result) table

argParseSpec :: (HasCallStack) => (Show a, Eq a) => [String] -> Parser a -> a -> Spec
argParseSpec args p expected = withFrozenCallStack $ do
  it (unwords ["parses args", show args, "as", show expected]) $ do
    let argMap = parseArgs args
    errOrRes <- runParserOn allCapabilities Nothing p argMap EnvMap.empty Nothing
    context (showParserABit p) $
      case errOrRes of
        Left errs ->
          expectationFailure $
            unlines
              [ T.unpack $ renderChunksText With24BitColours $ renderErrors errs,
                ppShow errs
              ]
        Right actual -> actual `shouldBe` expected

envParseSpecs :: (HasCallStack) => (Show a, Eq a) => Parser a -> [([(String, String)], a)] -> Spec
envParseSpecs p table = withFrozenCallStack $ mapM_ (\(envs, result) -> envParseSpec envs p result) table

envParseSpec :: (HasCallStack) => (Show a, Eq a) => [(String, String)] -> Parser a -> a -> Spec
envParseSpec envVars p expected = withFrozenCallStack $ do
  it (unwords ["parses environment", show envVars, "as", show expected]) $ do
    let envMap = EnvMap.parse envVars
    errOrRes <- runParserOn allCapabilities Nothing p emptyArgs envMap Nothing
    case errOrRes of
      Left err -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors err
      Right actual -> actual `shouldBe` expected

confParseSpecs :: (HasCallStack) => (Show a, Eq a) => Parser a -> [(Maybe JSON.Object, a)] -> Spec
confParseSpecs p table = withFrozenCallStack $ mapM_ (\(mConf, result) -> confParseSpec mConf p result) table

confParseSpec :: (HasCallStack) => (Show a, Eq a) => Maybe JSON.Object -> Parser a -> a -> Spec
confParseSpec mConf p expected = withFrozenCallStack $ do
  it (unwords ["parses configuration", show mConf, "as", show expected]) $ do
    errOrRes <- runParserOn allCapabilities Nothing p emptyArgs EnvMap.empty mConf
    case errOrRes of
      Left err -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors err
      Right actual -> actual `shouldBe` expected

shouldParse ::
  (Show a, Eq a) =>
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  a ->
  IO ()
shouldParse p = shouldParse' p allCapabilities

shouldParse' ::
  (Show a, Eq a) =>
  Parser a ->
  Capabilities ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  a ->
  IO ()
shouldParse' p capabilities args e mConf expected = do
  errOrRes <- runParserOn capabilities Nothing p args e mConf
  context (showParserABit p) $ case errOrRes of
    Left errs -> expectationFailure $ T.unpack $ renderChunksText With24BitColours $ renderErrors errs
    Right actual -> actual `shouldBe` expected

shouldFail ::
  (Show a) =>
  Parser a ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  (NonEmpty ParseErrorMessage -> Bool) ->
  IO ()
shouldFail p = shouldFail' p allCapabilities

shouldFail' ::
  (Show a) =>
  Parser a ->
  Capabilities ->
  Args ->
  EnvMap ->
  Maybe JSON.Object ->
  (NonEmpty ParseErrorMessage -> Bool) ->
  IO ()
shouldFail' p capabilities args e mConf isExpected = do
  errOrRes <- runParserOn capabilities Nothing p args e mConf
  case errOrRes of
    Left errs -> NE.map parseErrorMessage errs `shouldSatisfy` isExpected
    Right actual -> expectationFailure $ show actual

instance GenValid Capabilities

instance GenValid Capability
