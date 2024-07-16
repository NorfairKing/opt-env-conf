# Changelog

## [0.1.0.0] - 2024-07-16

### Changed

* `xdgYamlConfigFile` now returns a `Path Abs File` instead of a `FilePath`.
* Fixed a bug in `withFirstYamlConfig` and `withCombinedYamlConfig` in which the `--config-file` option was required.
* `--run-settings-check` now allows you to define a static settings check.
* Parse the combination of optional switches and optional arguments correctly.
* Fail to parse if any arguments are leftover.

## [0.0.0.0] - 2024-07-08

First version
