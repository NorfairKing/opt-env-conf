# OptEnvConf

## Status

Ready to try out,
Used in production

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

The [example application](./opt-env-conf-test/app/Main.hs) contains a fully worked example.

This example is part of the build in CI so you can rely on it being up-to-date.
