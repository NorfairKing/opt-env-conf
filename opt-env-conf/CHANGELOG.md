# Changelog

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
