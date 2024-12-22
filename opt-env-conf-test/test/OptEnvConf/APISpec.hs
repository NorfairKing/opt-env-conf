{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OptEnvConf.APISpec (spec) where

import Autodocodec
import Data.Map (Map)
import Data.Text (Text)
import Data.Version
import GHC.Stack (HasCallStack, withFrozenCallStack)
import OptEnvConf
import OptEnvConf.Test
import OptEnvConf.TestUtils
import Test.Syd
import Text.Colour
import Text.Show.Pretty as Pretty

spec :: Spec
spec = do
  exampleParserSpec "empty" "empty parser" emptyParser
  exampleParserSpec "many-args" "many args parser" manyArgsParser
  exampleParserSpec "some-args" "some args parser" someArgsParser
  exampleParserSpec "optional" "optional argument" optionalParser
  exampleParserSpec "big-config" "example with a big configuration" bigConfigParser
  exampleParserSpec "sub-settings" "example with a sub settings" subSettingsParser
  exampleParserSpec "hidden" "example with hidden settings" hiddenParser
  exampleParserSpec "enable-disable" "enableDisableSwitch example" enableDisableParser
  exampleParserSpec "yes-no" "yesNoSwitch example" yesNoParser
  exampleParserSpec "enable-disable-optional" "enableDisableSwitch' example" enableDisableParser'
  exampleParserSpec "yes-no-optional" "yesNoSwitch' example" yesNoParser'
  exampleParserSpec "verbose" "verbosity example" verboseParser
  exampleParserSpec "same-help" "example where multiple options use the same help string" sameHelpParser
  exampleParserSpec "sum-type" "Sum type example" sumTypeParser
  exampleParserSpec "secret" "Secrets example" secretParser
  exampleParserSpec "with-default" "withDefault example" withDefaultParser
  exampleParserSpec "greet" "hello world example" greetParser
  exampleParserSpec "three-commands" "example with three commands" threeCommandsParser
  exampleParserSpec "sub-commands" "example with subcommands" subCommandsParser

exampleParserSpec :: (HasCallStack) => FilePath -> String -> Parser a -> Spec
exampleParserSpec dir progDesc p = withFrozenCallStack $ describe dir $ do
  let version = makeVersion [0, 0, 0]

  it "passes the linter" $
    parserLintTest p

  it "shows the parser in the same way" $
    goldenStringFile ("test_resources/docs/" <> dir <> "/show.txt") $
      case Pretty.parseValue (showParserABit (parserEraseSrcLocs p)) of
        Nothing -> expectationFailure "Error parsing value"
        Just v -> pure $ Pretty.valToStr v

  it "produces the same docs structure as before" $
    pureGoldenStringFile ("test_resources/docs/" <> dir <> "/docs.txt") $
      ppShow $
        parserDocs p

  it "produces the same opt docs structure as before" $
    pureGoldenStringFile ("test_resources/docs/" <> dir <> "/opt-docs.txt") $
      ppShow $
        parserOptDocs p

  it "produces the same env docs structure as before" $
    pureGoldenStringFile ("test_resources/docs/" <> dir <> "/env-docs.txt") $
      ppShow $
        parserConfDocs p

  it "produces the same conf docs structure as before" $
    pureGoldenStringFile ("test_resources/docs/" <> dir <> "/config-docs.txt") $
      ppShow $
        parserEnvDocs p

  it "documents the version page in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/version.txt") $
      renderVersionPage dir version

  it "documents the help page in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/help.txt") $
      renderHelpPage dir version progDesc $
        parserDocs p

  it "documents the short opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/opt-short.txt") $
      renderShortOptDocs dir $
        parserOptDocs p

  it "documents the long opt parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/opt-long.txt") $
      renderLongOptDocs $
        parserOptDocs p

  it "documents the env parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/env.txt") $
      renderEnvDocs $
        parserEnvDocs p

  it "documents the conf parser in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/config.txt") $
      renderConfDocs $
        parserConfDocs p

  it "documents the man page in the same way" $
    pureGoldenTextFile ("test_resources/docs/" <> dir <> "/man.txt") $
      renderChunksText WithoutColours $
        renderManPage dir version progDesc $
          parserDocs p

  it "renders the reference documentation in the same way" $
    pureGoldenChunksFile ("test_resources/docs/" <> dir <> "/reference.txt") $
      renderReferenceDocumentation dir $
        parserDocs p

  it "renders the Nix options the same way" $
    pureGoldenTextFile ("test_resources/docs/" <> dir <> "/nix-options.nix") $
      renderParserNixOptions p

sameHelpParser :: Parser (Either Int String, Bool)
sameHelpParser =
  withoutConfig $
    let h = help "int or string"
     in (,)
          <$> choice
            [ Left
                <$> setting
                  [ h,
                    name "int",
                    reader auto,
                    metavar "INT"
                  ],
              Right
                <$> setting
                  [ h,
                    name "string",
                    reader str,
                    metavar "STR"
                  ]
            ]
          <*> yesNoSwitch
            [ h,
              name "other",
              value True
            ]

data SumType
  = SumTypeA
  | SumTypeB
  deriving (Show)

instance HasCodec SumType where
  codec = bimapCodec parseSumType renderSumType codec

parseSumType :: String -> Either String SumType
parseSumType = \case
  "a" -> pure SumTypeA
  "b" -> pure SumTypeB
  s -> Left $ unwords ["Unknown 'SumType':", s]

renderSumType :: SumType -> String
renderSumType = \case
  SumTypeA -> "a"
  SumTypeB -> "b"

sumTypeParser :: Parser SumType
sumTypeParser =
  withoutConfig $
    let h = help "example"
     in choice
          [ setting
              [ help "a",
                switch SumTypeA,
                short 'a'
              ],
            setting
              [ help "b",
                switch SumTypeB,
                short 'b'
              ],
            setting
              [ h,
                reader $ eitherReader parseSumType,
                env "sum-type",
                metavar "SUM_TYPE"
              ],
            setting
              [ h,
                conf "sum-type"
              ],
            setting
              [ h,
                valueWithShown renderSumType SumTypeA
              ]
          ]

secretParser :: Parser (Text, Text, Text, Text, Text)
secretParser =
  withoutConfig $
    (,,,,)
      <$> secretTextFileSetting
        [ help "First example secret",
          name "first-secret-file"
        ]
      <*> secretTextFileOrBareSetting
        [ help "Second example secret, bare or in a file",
          name "second-secret"
        ]
      <*> secretTextFileOrBareSetting
        [ help "Second example secret, bare or in a file, only option",
          option,
          long "second-secret"
        ]
      <*> secretTextFileOrBareSetting
        [ help "Second example secret, bare or in a file, only env",
          env "second-secret"
        ]
      <*> secretTextFileOrBareSetting
        [ help "Second example secret, bare or in a file, only conf",
          conf "second-secret"
        ]

data FirstOrSecond = First | Second
  deriving (Show)

instance HasParser FirstOrSecond where
  settingsParser =
    -- This version has no default value, so we can add one with 'withDefault'
    choice
      [ setting
          [ help "first",
            switch First,
            short 'f',
            long "first"
          ],
        setting
          [ help "second",
            switch Second,
            short 's',
            long "second"
          ]
      ]

withDefaultParser :: Parser FirstOrSecond
withDefaultParser = withDefault Second settingsParser

data Greet = Greet !String !String !Bool

greetParser :: Parser Greet
greetParser =
  subEnv "GREET_" $
    withLocalYamlConfig $
      Greet
        <$> setting
          [ reader str,
            option,
            short 'g',
            long "greeting",
            metavar "GREETING",
            env "GREETING",
            conf "greeting",
            value "Hello",
            example "Greetings",
            help "Greeting to use"
          ]
        <*> setting
          [ reader str,
            argument,
            help "Who to greet",
            value "world",
            metavar "SUBJECT"
          ]
        <*> setting
          [ reader exists,
            switch True,
            short 'p',
            long "polite",
            env "POLITE",
            conf "polite",
            metavar "ANY",
            value False,
            help "Whether to be polite"
          ]

data BigConfig = BigConfig
  { bigConfigMap :: Map String (Map String Int),
    bigConfigSubObject :: Maybe Text
  }

instance HasCodec BigConfig where
  codec =
    object "BigConfig" $
      BigConfig
        <$> requiredField' "map" .= bigConfigMap
        <*> optionalField' "sub" .= bigConfigSubObject

bigConfigParser :: Parser BigConfig
bigConfigParser =
  withLocalYamlConfig $
    setting
      [ conf "big",
        help "big configuration object"
      ]

subSettingsParser :: Parser String
subSettingsParser =
  withLocalYamlConfig $
    subAll "foo" $
      subAll "bar" $
        setting
          [ reader str,
            name "quux",
            help "Example with sub-settings",
            metavar "STR"
          ]

manyArgsParser :: Parser [String]
manyArgsParser =
  many
    ( setting
        [ reader str,
          argument,
          help "Argument",
          metavar "ARGUMENT"
        ]
    )

someArgsParser :: Parser [String]
someArgsParser =
  some
    ( setting
        [ reader str,
          argument,
          help "Argument",
          metavar "ARGUMENT"
        ]
    )

data Optional = Optional (Maybe String)

optionalParser :: Parser Optional
optionalParser =
  Optional
    <$> optional
      ( setting
          [ reader str,
            argument,
            help "Argument",
            metavar "ARGUMENT"
          ]
      )

data Hidden = Hidden String

hiddenParser :: Parser Hidden
hiddenParser =
  Hidden
    <$> setting
      [ reader str,
        argument,
        hidden,
        metavar "STR",
        value "default",
        help "Example of a hidden setting"
      ]

data EnableDisable = EnableDisable Bool

enableDisableParser :: Parser EnableDisable
enableDisableParser =
  withoutConfig $
    EnableDisable
      <$> enableDisableSwitch
        [ long "example",
          help "Example of an enable/disable switch",
          env "EXAMPLE",
          conf "example",
          value True
        ]

enableDisableParser' :: Parser (Maybe EnableDisable)
enableDisableParser' =
  withoutConfig $
    fmap EnableDisable
      <$> optional
        ( enableDisableSwitch
            [ long "example",
              help "Example of an enable/disable switch",
              env "EXAMPLE",
              conf "example"
            ]
        )

data YesNo = YesNo Bool

yesNoParser :: Parser YesNo
yesNoParser =
  withoutConfig $
    YesNo
      <$> yesNoSwitch
        [ long "example",
          help "Example of a yes/no switch",
          env "EXAMPLE",
          conf "example",
          value True
        ]

yesNoParser' :: Parser (Maybe YesNo)
yesNoParser' =
  withoutConfig $
    fmap YesNo
      <$> optional
        ( yesNoSwitch
            [ long "example",
              help "Example of a yes/no switch",
              env "EXAMPLE",
              conf "example"
            ]
        )

data Empty = Empty

emptyParser :: Parser Empty
emptyParser =
  pure Empty

data ThreeCommands
  = One !String
  | Two !Int !Bool
  | Three

threeCommandsParser :: Parser ThreeCommands
threeCommandsParser =
  withoutConfig $
    commands
      [ command "one" "first" $
          One
            <$> setting
              [ help "argument",
                reader str,
                metavar "STR",
                argument
              ],
        command "two" "second" $
          Two
            <$> setting
              [ help "number",
                reader auto,
                option,
                metavar "INT",
                name "number",
                short 'n',
                shownExample 5
              ]
            <*> enableDisableSwitch
              [ help "enable extra",
                name "enable",
                value False
              ],
        command "three-very-long-command-name" "third" (pure Three),
        defaultCommand "one"
      ]

data SubCommands
  = Top !String
  | Sub !Sub1 !Sub2

subCommandsParser :: Parser SubCommands
subCommandsParser =
  withLocalYamlConfig $
    commands
      [ command "top" "command without subcommands" $
          Top
            <$> setting
              [ help "name",
                reader str,
                metavar "NAME",
                name "name",
                example "john"
              ],
        command "sub" "command with subcommands" $
          Sub <$> sub1Parser <*> sub2Parser
      ]

data Sub1 = A | B

sub1Parser :: Parser Sub1
sub1Parser =
  commands
    [ command "a" "A" $ pure A,
      command "b" "B" $ pure B
    ]

data Sub2 = C | D

sub2Parser :: Parser Sub2
sub2Parser =
  commands
    [ command "c" "C" $ pure C,
      command "d" "D" $ pure D,
      defaultCommand "c"
    ]

verboseParser :: Parser Int
verboseParser =
  length
    <$> many
      ( setting
          [ help "Verbosity level. Use multiple to increase verbosity",
            short 'v',
            switch ()
          ]
      )
