# Changelog for rollbar-client

All notable changes to this project will be documented in this file.

## [1.2.0] - 2026-08-18

### Changed

- **Breaking:** `mkException` now sets the exception class to the exception
  type name (unwrapping `SomeException` and `SomeAsyncException`) instead of
  the rendered exception text, which embeds per-occurrence data and created a
  new Rollbar item for nearly every occurrence. The rendered text is kept as
  the message (first line) and the description (in full).

  Upgrading re-groups existing items once: occurrences reported after the
  upgrade carry new class values, so Rollbar files them under new items.

  Traces still carry no stack frames, so all occurrences of one exception type
  group into a single item; set `fingerprint` on the `Item` for finer-grained
  grouping.

## [1.1.0] - 2024-05-28

### Changed
- Added fields fingerprint, title, uuid, custom to `Item`
- Added support for GHC 9.4
- Changed `text` dependency upper bound: we now support `text-2.0.X.X`.

## [1.0.0] - 2022-12-28

### Changed

- Updated dependency aeson version.
- Updated base version

### Removed

- Support for GHC 8.6.1
