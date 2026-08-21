# Changelog for rollbar-yesod

All notable changes to this project will be documented in this file.

## [1.2.0] - 2026-08-18

### Changed

- **Breaking:** captured exceptions are now reported with the exception type
  name as the Rollbar class and the rendered text as message and description.
  Upgrading re-groups existing items once; see the rollbar-client 1.2.0 entry.
  Requires rollbar-client >= 1.2 and rollbar-wai >= 1.2.

## [1.1.0] - 2024-05-28

### Changed
- Added support for GHC 9.4

## [1.0.0] - 2022-12-28

### Changed

- Updated dependency aeson version.
- Updated base version

### Removed

- Support for GHC 8.6.1
