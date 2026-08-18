# Changelog for rollbar-wai

All notable changes to this project will be documented in this file.

## [1.2.0] - 2026-08-18

### Changed

- **Breaking:** exceptions captured by `rollbarOnException` and
  `rollbarOnExceptionWith` are now reported with the exception type name as the
  Rollbar class, and the rendered exception text as the message and description.
  Upgrading re-groups existing items once; see the rollbar-client 1.2.0 entry
  for the details. This requires rollbar-client >= 1.2.

## [1.1.0] - 2024-05-28

### Changed
- Added support for GHC 9.4
- Changed `text` dependency upper bound: we now support `text-2.0.X.X`.

## Unreleased changes
