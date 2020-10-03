# Rollbar Haskell

A group of libraries written in Haskell to communicate with [Rollbar
API](https://explorer.docs.rollbar.com/).

- [rollbar-cli](rollbar-cli/) - Simple CLI tool to perform commons tasks such
  as tracking deploys.
- [rollbar-client](rollbar-client/) - Core library to communicate with Rollbar
  API.
- [rollbar-wai](rollbar-wai/) - Provides some functions to integrate Rollbar
  with [wai](http://hackage.haskell.org/package/wai) based applications.

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
