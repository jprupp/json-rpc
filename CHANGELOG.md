# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 1.1.2 - 2025-05-09

### Fixed
- Correct Arbitraty instance for errors.

## 1.1.1 - 2023-03-14

### Fixed
- Close TBMChan when the session is closed

## 1.1.0 - 2023-12-29

### Changed
- Update dependency to Aeson 2.2.

## 1.0.4 - 2022-05-15

### Fixed
- Remove `Arbitrary Value` instance because it is already provided by the `aeson` package.

### Changed
- Relax restrictions on `params` field for JSON-RPCv1 requests.
- Bump minimum version of `aeson` accordingly.

## 1.0.3 - 2020-06-19

### Changed
- `LogSource` (defined in `monad-logger`) for this library is set to `"json-rpc"` so that logs can be filtered base on it.

## 1.0.2 - 2020-06-12
### Changed
- Change license to MIT.

## 1.0.1 - 2019-01-01
### Fixed
- Correct JSON-RPC 2.0 methods returning null result.

## 1.0.0 - 2018-08-12
### Added
- Complete JSON-RPC 1.0 and 2.0 support.
- Ability to use either endpoint to send and receive requests, responses or notifications.
- Simple TCP client/server implementation available.
- Example files.
- Exhaustive test suite.
- Compatibility with GHC 8.4.
- Support semantic versioning.
- Add a changelog.
