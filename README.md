# Rollbar Haskell

![CI](https://github.com/stackbuilders/rollbar-haskell/workflows/CI/badge.svg)

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

- Install one of the following set of tools (or all of them):
  - [GHC](https://www.haskell.org/ghc/download.html) and
    [cabal](https://www.haskell.org/cabal/download.html).
  - [stack](https://docs.haskellstack.org/en/stable/README/).
- Set up a [Rollbar][rollbar] account, create a project, and generate an access
  token.
  - To get an access token, go to your project's Settings and then to Project
    Access Tokens, where you can copy or create an access token with scope
    `post_server_item`.

[rollbar]: https://rollbar.com/

## Getting Started

### Cabal

Compile the projects:

```
cabal update
cabal configure --enable-tests
cabal build all
```

Run all tests:

```
env ROLLBAR_TOKEN=<token> cabal test all
```

### Stack

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

[rollbar-api]: https://docs.rollbar.com/
