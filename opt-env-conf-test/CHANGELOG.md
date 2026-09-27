# Changelog

## [0.0.0.5] - 2026-09-27

### Changed

* The completer tests now expect the sorted completion output that
  `opt-env-conf >=0.15.0.3` produces, and the test suite requires that version.
  These test changes were made together with the `opt-env-conf` fix but never
  released, so the released tests still expected the order the filesystem
  happened to report and failed on machines that list directories differently.

## [0.0.0.4] - 2026-02-25

### Changed

* `pureGoldenNixOptions` now prepends a generated file comment pointing to the test that generated each file

## [0.0.0.3] - 2025-11-20

### Added

* Compabitility with `opt-env-conf >=0.13`

## [0.0.0.2] - 2025-03-12

### Added

* Completions test helpers


## [0.0.0.1] - 2024-08-08

### Added

* `settingsParserArgsTest`
* `parserArgsTest`
* `settingsParserEnvTest`
* `parserEnvTest`
* `settingsParserConfTest`
* `parserConfTest`
* `settingsParserTest`
* `parserTest`

## [0.0.0.0] - 2024-07-08

First version
