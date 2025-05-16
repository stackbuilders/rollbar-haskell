# Changelog for rollbar-wai

All notable changes to this project will be documented in this file.

## [1.1.0] - 2024-05-28

### Changed
- Added support for GHC 9.4
- Changed `text` dependency upper bound: we now support `text-2.0.X.X`.

## Unreleased changes

### Changed
- `rollbarOnExceptionWith` now takes a fork function of type `IO () -> IO ()`.
- All tests are now synchronous and no longer use `threadDelay`.
- Updated `rollbarOnException` and all usages to match the new signature.
