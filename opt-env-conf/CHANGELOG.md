# Changelog

## [Unreleased] -- 2024-07-16

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
