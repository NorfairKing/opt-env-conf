# OptEnvConf

[![NixCI](https://staging.nix-ci.com/badge/gh:NorfairKing:opt-env-conf)](https://staging.nix-ci.com/gh:NorfairKing:opt-env-conf)

## Status

Used in production in all my products and some companies.

## Goals

* Parse command-line arguments, environment variables, and configuration values all together.
* Self-documenting parsers for correct-by-construction documentation
* Best-in-class command-line autocompletion
* Best-in-class errors
* Formatter-friendly API

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
| Bash completion                      | ✔️              | ✔️                      | ✖️          | ✖️             |
| Zsh completion                       | ✔️              | ✔️                      | ✖️          | ✖️             |
| Fish completion                      | ✔️              | ✔️                      | ✖️          | ✖️             |
| Static settings check                | ✔️              | ✖️                      | ✖️          | ✖️             |

## Parsers and Interpreters

The essence of `opt-env-conf` is the concept of a *Parser*.
Parsers are a tree of settings that can be interpreted in different ways.
Currently, the following interpreters are provided:

* Parse the settings
* Lint the parser for common issues at runtime that were inconvenient to catch at the type level.
* Run a settings check (possibly with limited capabilities)
* Generate documentation, including a man page, a `--help` page, and documentation for each command recursively as well.
* Generate shell completions for Bash, Zsh, and Fish
* Generate a NixOS Option type

## Example

The [example application](./opt-env-conf-example) contains a fully worked example.

This example is part of the build in CI so you can rely on it being up-to-date.
