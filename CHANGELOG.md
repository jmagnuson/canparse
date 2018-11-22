# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com)
and this project adheres to [Semantic Versioning](http://semver.org).

## [Unreleased]

### Changed
- Change `get_spn` to take `&str` input
- Speed up parsing PGN from `&[u8]`

### Fixed
- Explicitly parse file passed into `PgnLibrary::from_dbc_file` using
  ISO-8859-1 codec

## [0.1.2] - 2018-06-09

### Changed
- Improve Error types for various parse failures
- Remove unnecessary re-exports

### Fixed
- Fix `socketcan`-specific build failures

## [0.1.1] - 2018-05-22

[Unreleased]: https://github.com/jmagnuson/canparse/compare/v0.1.2...master
[0.1.2]: https://github.com/jmagnuson/canparse/compare/v0.1.1...v0.1.2
