# Rollbar Haskell

[![CircleCI](https://circleci.com/gh/stackbuilders/rollbar-haskell.svg?style=shield&circle-token=8006d81b2704e6649dddd32146493054b3033bc2)](https://app.circleci.com/pipelines/github/stackbuilders/rollbar-haskell)

A group of libraries written in Haskell to communicate with [Rollbar
API][rollbar-api]. Inspired by
[bugsnag-haskell](https://github.com/pbrisbin/bugsnag-haskell).

- [rollbar-cli](rollbar-cli/) - Simple CLI tool to perform commons tasks such
  as tracking deploys.
- [rollbar-client](rollbar-client/) - Core library to communicate with [Rollbar
  API][rollbar-api].
- [rollbar-wai](rollbar-wai/) - Provides error reporting capabilities to
  [WAI](http://hackage.haskell.org/package/wai) based applications through
  [Rollbar API][rollbar-api].
- [rollbar-yesod](rollbar-yesod/) - Provides error reporting capabilities to
  [Yesod](https://www.yesodweb.com/) applications through [Rollbar
  API][rollbar-api].

## Requirements

- Install [stack](https://docs.haskellstack.org/en/stable/README/).
- Setup a Rollbar account, create a project and generate an access token.

## Getting Started

Compile the projects:

```
stack build
```

Run all tests:

```
env ROLLBAR_TOKEN=<token> stack test
```

## License

[MIT](LICENSE)

[rollbar-api]: https://explorer.docs.rollbar.com/
