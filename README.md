# OptEnvConf ![Hackage](https://img.shields.io/hackage/v/opt-env-conf.svg) ![Hackage-Deps](https://img.shields.io/hackage-deps/v/opt-env-conf.svg)

Parse and merge settings for your web app or CLI tool from all typical sources like 
**configuration files, command-line arguments and environment variables** all with one convenient library.

## Quickstart

<details>
<summary>
Click to show the .cabal file part.
Learn more at <a href="https://cs-syd.eu/posts/2024-07-08-announcing-opt-env-conf" target="_blank">https://cs-syd.eu/posts/2024-07-08-announcing-opt-env-conf</a>
</summary>

```cabal
executable myApp
  build-depends:
    ...
    , opt-env-conf
```

</details>
<p></p>

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (Text)
import OptEnvConf
import Data.Version (Version, makeVersion)

########################## Usage

appVersion :: Version
appVersion = makeVersion [1, 0, 0]

main :: IO ()
main = do
  settings <- runSettingsParser appVersion "Run the foo-bar server"
  runServer settings

runServer :: Settings -> IO ()
runServer = do
  putStrLn "Parsing worked!"

########################## Settings definitions

data Settings = Settings
  { settingPort :: Int,
    settingPaymentSettings :: Maybe PaymentSettings
  }

data PaymentSettings = PaymentSettings
  { paymentSettingPublicKey :: Text,
    paymentSettingPrivateKey :: Text
  }

########################## Parsing configuration

instance HasParser Settings where
  settingsParser = subEnv "FOO_BAR_" $ withLocalYamlConfig $ do
    settingPort <-
      setting
        [ help "the port to serve web requests on",
          reader auto,
          option,
          long "port",
          env "PORT",
          conf "port",
          metavar "PORT",
          value 8080
        ]
    settingPaymentSettings <- optional $ subSettings "payment"
    pure Settings {..}

instance HasParser PaymentSettings where
  settingsParser = do
    paymentSettingPublicKey <-
      setting
        [ help "public key",
          reader str,
          name "public-key",
          metavar "PUBLIC_KEY"
        ]
    paymentSettingPrivateKey <-
      mapIO readSecretTextFile $
        filePathSetting
          [ help "private key file",
            reader str,
            name "private-key-file",
            metavar "PRIVATE_KEY_FILE"
          ]
    pure PaymentSettings {..}
```

## Status

Used in production in all my products and some companies.

## Goals

* Parse command-line arguments, environment variables, and configuration values all together.
* Self-documenting parsers for correct-by-construction documentation
* Best-in-class command-line autocompletion
* Best-in-class errors
* Formatter-friendly API

## Features

- [x] Parsing
    - [x] Argument: `progname hello`
    - [x] Option: `progname --file foo.txt`
        - [x] Long Option: `progname --file foo.txt`
        - [x] Short Option: `progname --file foo.txt`
        - [x] Equals-version of long option: `progname --file=foo.txt`
        - [x] Shorthand-version of short option: `progname -ffoo.txt`
    - [x] Switch: `progname --verbose`
        - [x] Long switch: `progname --verbose`
        - [x] Short switch: `progname -v`
- [ ] Documentation
    - [ ] `--help`
        - [x] Global `--help` page
        - [ ] Per-command `--help` page
    - [x] Generated `--version` command
    - [x] Generated manpage
- [ ] Completion
    - [ ] Bash completion
    - [ ] Zsh completion
    - [ ] Fish completion
- [x] Static settings check

## Comparison to similar projects

|                                      | `opt-env-conf` | `optparse-applicative` | `envparse` | `autodocodec` |
|--------------------------------------|----------------|------------------------|------------|---------------|
| Applicative parsing                  | ✔️              | ✔️                      | ✔️          | ✔️             |
| Parsing arguments                    | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing long options                 | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing short options                | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing short-hand short options     | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing short-hand long options      | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing long switches                | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing short switches               | ✔️              | ✔️                      | ✖️          | ✖️             |
| Parsing environment variables        | ✔️              | ✖️                      | ✔️          | ✖️             |
| Parsing configuration values         | ✔️              | ✖️                      | ✖️          | ✔️             |
| Generated global `--help` page       | ✔️              | ✔️                      | ✖️          | ✖️             |
| Coloured global `--help` page        | ✔️              | ✖️                      | ✖️          | ✖️             |
| Generated per-command `--help` page  | ✔️              | ✔️                      | ✖️          | ✖️             |
| Coloured per-command `--help` page   | ✔️              | ✖️                      | ✖️          | ✖️             |
| Generated `--version` command        | ✔️              | ✖️                      | ✖️          | ✖️             |
| Generated manpage                    | ✔️              | ✖️                      | ✖️          | ✖️             |
| Helpful parse errors                 | ✔️              | ✔️                      | ✔️          | ✔️             |
| Coloured parse errors                | ✔️              | ✖️                      | ✖️          | ✖️             |
| Generated manpage                    | ✔️              | ✖️                      | ✖️          | ✖️             |
| Typo suggestions                     | 🚧             | ✖️                      | ✖️          | ✖️             |
| Bash completion                      | 🚧             | ✔️                      | ✖️          | ✖️             |
| Zsh completion                       | 🚧             | ✔️                      | ✖️          | ✖️             |
| Fish completion                      | 🚧             | ✔️                      | ✖️          | ✖️             |
| Static settings check                | ✔️              | ✖️                      | ✖️          | ✖️             |


## Example

The [example application](./opt-env-conf-example) contains a fully worked example.

This example is part of the build in CI so you can rely on it being up-to-date.
