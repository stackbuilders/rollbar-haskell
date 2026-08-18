# Changelog for rollbar-client

All notable changes to this project will be documented in this file.

## [1.2.0] - 2026-08-18

### Changed

- **Breaking:** `mkException` now sets the exception class to the name of the
  exception type, unwrapping the `SomeException` and `SomeAsyncException`
  wrappers first, instead of the rendered exception text. The rendered text is
  kept as the message (its first line, when non-blank) and as the description
  (in full), so no information is lost. Rollbar groups trace
  payloads by class, and rendered exceptions routinely embed per-occurrence data
  such as urls, ids and call stacks, so the previous behaviour created a new
  item for nearly every occurrence instead of one per failure cause.

  Upgrading re-groups existing items once: occurrences reported after the
  upgrade carry new class values, so Rollbar files them as new items rather than
  adding to the ones already open.

  Known limitation: this library does not populate stack frames yet, so
  Rollbar's default fingerprint (frame filenames and methods plus the exception
  class) reduces to the class alone and all occurrences of one exception type
  are grouped into a single item. Applications that need finer-grained grouping
  can set the `fingerprint` field on the `Item` before sending it, for example
  through `rollbarOnExceptionWith`. The exception message is not part of
  Rollbar's default fingerprint, but projects that enabled Rollbar's "include
  message in fingerprint" setting will see items split per distinct message.

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
