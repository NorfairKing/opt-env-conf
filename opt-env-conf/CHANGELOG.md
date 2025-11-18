# Changelog

## [0.12.2.0] - 2025-11-18

### Changed

* Fixed a bug in which parse errors would be ignored during checking as soon as
  one missing capability was encountered.

## [0.12.0.0] - 2025-11-15

### Added

* Support for capabilities during settings parsing, and disabling them in the
  settings check.

This is technically a breaking change, but if you don't use any `opt-env-conf`
internals, nothing should break for you.

## [0.11.1.0] - 2025-10-23

### Added

* Windows support by isolating platform-specific terminal code.

## [0.11.0.0] - 2025-09-29

This is technically a breaking change, but if you don't use any `opt-env-conf`
internals, nothing should break for you.

### Changed

* `ParserMany` and `ParserSome` can now carry source locations.
  This is not likely to happen often because the `Alternative` class does not
  have a `HasCallStack` super class, but could still happen when using
  `someNonEmpty`.

## [0.10.0.0] - 2025-09-17

This is technically a breaking change, but if you don't use any `opt-env-conf`
internals, nothing should break for you.

### Added

* `unprefixedEnv`
* `unprefixedConf`

## [0.9.0.0] - 2025-03-12

### Added

* Completions

## [0.8.0.1] - 2024-12-22

### Changed

* Fixed that `secretTextFileOrBareSetting` would not pass the linter without
  `name`.

## [0.8.0.0] - 2024-11-05

### Changed

* Change `withShownDefault` and `valueWithShown` to accept a function to use in
  place of `show` rather than a pre-rendered `String`.

## [0.7.0.1] - 2024-10-27

### Changed

* Fixed a bug in which `allOrNothing` (and `subSettings`) could malfunction
  when using a common library setting multiple times.

## [0.7.0.0] - 2024-10-27

### Added

* The `allOrNothing` error now shows which settings were defined.

## [0.6.0.4] - 2024-10-26

### Added

* Added a lint for an unknown default command.

## [0.6.0.3] - 2024-10-24

### Added

* `withDefault` and `withShownDefault`.

## [0.6.0.2] - 2024-10-20

### Changed

* Fixed that configuration parsing errors were double-`show`n.

## [0.6.0.1] - 2024-10-18

### Added

* `secretTextFileSetting`
* `secretTextFileOrBareSetting`

## [0.6.0.0] - 2024-10-18

### Added

* `defaultCommand`

### Changed

* `commands` now takes `CommandsBuilder`s.

## [0.5.1.1] - 2024-08-08

### Changed

* Fixed a bug in which a `setting` with only a default `value` was considered invalid.

## [0.5.1.0] - 2024-08-08

### Removed

* Removed the "undecodable example" lint because it was faulty when example values aren't Strings.

## [0.5.0.1] - 2024-08-04

### Changed

* Errors involving commands now show command descriptions.

## [0.5.0.0] - 2024-08-03

### Changed

* Made `Builder`s contain a list of `BuildInstructions` so library functions
  can pattern-match on the builders.
* `yesNoSwitch` and `enableDisableSwitch` now no longer take a default value,
  but accept default `value` builders instead.
* Renamed 'viaCodec' to 'viaStringCodec'.
* Clearer lints when using `name`.
* Clearer lint about loading configuration.
* Default values are now shown at the end but before example values in documentation.
* Proper `[--optional-option-docs]`

## [0.4.0.5] - 2024-07-28

### Added

* Documentation now shows example values and default values.

## [0.4.0.4] - 2024-07-28

### Added

* Added a lint for examples that none of the configuration codecs can decode.

### Changed

* Fixed: linter would 'catch' unreadable examples even though a setting would only read a configuration value

## [0.4.0.3] - 2024-07-28

### Changed

* Support (commands [..] <|> commands [..]) by concat-ing the lists of commands.
* Improved the documentation of parsers that use `some`.

## [0.4.0.2] - 2024-07-27

### Changed

* Fixed a bug in which unrecognised arguments would be parsed exponentially.
* The special `--version` command no longer allows any other arguments.
* Fixed a bug in which some source locations still showed up even though debug mode was not on.
* Fixed that only one codec for a configuration setting was tried.

## [0.4.0.1] - 2024-07-26

### Added

* The `viaCodec` reader.

## [0.4.0.0] - 2024-07-23

### Added

* Added a hidden `--render-reference-documentation` command.
* Added a per-command `--help` page.

### Changed

* Changed the name of `mkSettingsCheck` to `makeSettingsCheck`.

## [0.3.0.0] - 2024-07-19

### Changed

* Debug mode now provides much more info about all the settings that were and weren't parsed, and why.
* Fixed a bug in which arguments that were consumed in a failed branch were not being unconsumed afterwards.
* Nix functions now produce completion as well as man pages.

## [0.2.0.0] -- 2024-07-18

### Changed

* Fixed that the settings check could not be run with arguments.
* Added a lint to check that `long` isn't used without `option` or `switch`.
* Added a lint to check that `many` cannot be used with a parser that can succeed without consuming anything.

## [0.1.0.0] - 2024-07-16

### Changed

* `xdgYamlConfigFile` now returns a `Path Abs File` instead of a `FilePath`.
* Fixed a bug in `withFirstYamlConfig` and `withCombinedYamlConfig` in which the `--config-file` option was required.
* `--run-settings-check` now allows you to define a static settings check.
* Parse the combination of optional switches and optional arguments correctly.
* Fail to parse if any arguments are leftover.

## [0.0.0.0] - 2024-07-08

First version
