# Rollbar Haskell
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-1-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

![CI](https://github.com/stackbuilders/rollbar-haskell/workflows/CI/badge.svg)
[![Hackage Version](https://img.shields.io/hackage/v/rollbar-cli?label=rollbar-cli)](http://hackage.haskell.org/package/rollbar-cli)
[![Hackage Version](https://img.shields.io/hackage/v/rollbar-client?label=rollbar-cli)](http://hackage.haskell.org/package/rollbar-client)
[![Hackage Version](https://img.shields.io/hackage/v/rollbar-wai?label=rollbar-cli)](http://hackage.haskell.org/package/rollbar-wai)
[![Hackage Version](https://img.shields.io/hackage/v/rollbar-yesod?label=rollbar-cli)](http://hackage.haskell.org/package/rollbar-yesod)

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

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)

[rollbar-api]: https://docs.rollbar.com/

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="http://www.jpvillaisaza.com"><img src="https://avatars.githubusercontent.com/u/584947?v=4?s=100" width="100px;" alt="Juan Pedro Villa Isaza"/><br /><sub><b>Juan Pedro Villa Isaza</b></sub></a><br /><a href="https://github.com/stackbuilders/rollbar-haskell/commits?author=jpvillaisaza" title="Code">💻</a> <a href="#example-jpvillaisaza" title="Examples">💡</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/cptrodolfox"><img src="https://avatars.githubusercontent.com/u/20303685?v=4?s=100" width="100px;" alt="William R. Arellano"/><br /><sub><b>William R. Arellano</b></sub></a><br /><a href="https://github.com/stackbuilders/rollbar-haskell/commits?author=cptrodolfox" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/sestrella"><img src="https://avatars.githubusercontent.com/u/2049686?v=4?s=100" width="100px;" alt="Sebastián Estrella"/><br /><sub><b>Sebastián Estrella</b></sub></a><br /><a href="https://github.com/stackbuilders/rollbar-haskell/commits?author=sestrella" title="Code">💻</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!