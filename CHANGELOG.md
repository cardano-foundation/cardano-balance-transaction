# Changelog

## 1.0.0 (2026-04-17)


### ⚠ BREAKING CHANGES

* remove cardano-api from test dependencies
* CardanoApiEra, toCardanoApiTx, fromCardanoApiTx, cardanoEraFromRecentEra, shelleyBasedEraFromRecentEra removed from public API.

### Features

* add cardano-ledger-dijkstra dependency ([4f81ddf](https://github.com/cardano-foundation/cardano-balance-transaction/commit/4f81ddf8e82251a1a7313070f25698e7172184b6))
* add Dijkstra CBOR round-trip golden tests ([b83d41c](https://github.com/cardano-foundation/cardano-balance-transaction/commit/b83d41cfc77d4b44f06a302646c27d0d0afa529c))
* bump dependencies to cardano-node 10.6.2 ([f23dbb9](https://github.com/cardano-foundation/cardano-balance-transaction/commit/f23dbb90cfec31b6bc81a0f64f692222e2f64ee1)), closes [#7](https://github.com/cardano-foundation/cardano-balance-transaction/issues/7)
* copy balance-tx source with namespace rename ([00a13d1](https://github.com/cardano-foundation/cardano-balance-transaction/commit/00a13d1e3b1d3805cf94872f4a7a24b1a46ef0ff))
* enable Dijkstra era tests ([eb26c6f](https://github.com/cardano-foundation/cardano-balance-transaction/commit/eb26c6f52f577b95e3b1f0e44fdded4422bf8d8d))
* remove all wallet dependencies from library and test suite ([4f4d1d7](https://github.com/cardano-foundation/cardano-balance-transaction/commit/4f4d1d76c8ab976159368440ec8ae7bb6af37ba8))
* replace wallet-primitive types with standalone Primitive module ([bcff708](https://github.com/cardano-foundation/cardano-balance-transaction/commit/bcff708ae0e292a7e5f86494726ed2c3a7768959))
* repository skeleton with extraction plan ([9ff2f23](https://github.com/cardano-foundation/cardano-balance-transaction/commit/9ff2f23d75119b1dfbcf262d820a69056485b563))


### Bug Fixes

* adapt RecentEraConstraints to ledger 1.19 API ([7cda7e3](https://github.com/cardano-foundation/cardano-balance-transaction/commit/7cda7e35e83c36e5b8bf703f4b61c483202bb237)), closes [#37](https://github.com/cardano-foundation/cardano-balance-transaction/issues/37)
* add CHaP flake input to avoid DNS resolution in Nix sandbox ([ec74279](https://github.com/cardano-foundation/cardano-balance-transaction/commit/ec7427959a97ef78e709d395e8118ffae850e73b))
* add EqRaw (NativeScript era) to RecentEraConstraints ([67dcacd](https://github.com/cardano-foundation/cardano-balance-transaction/commit/67dcacdea6e83a367fc2e9c776a6f53c39e40058))
* add SafeToHash (NativeScript era) to RecentEraConstraints ([5d69cc9](https://github.com/cardano-foundation/cardano-balance-transaction/commit/5d69cc9bd47062b363929c877b83f0ab96369583)), closes [#39](https://github.com/cardano-foundation/cardano-balance-transaction/issues/39)
* enable -Werror and fix all warnings ([a069da8](https://github.com/cardano-foundation/cardano-balance-transaction/commit/a069da87ad0ced93f1f9e66061f1e9228a881f59))
* hardcode test data path so nix run works ([27c7600](https://github.com/cardano-foundation/cardano-balance-transaction/commit/27c7600a0f0ae60dd30f95308446b354be5010e5))
* migrate mkdocs gh-deploy to mkdocs-deploy wrapper ([0c71486](https://github.com/cardano-foundation/cardano-balance-transaction/commit/0c71486efac80be6d23e472adcbbc2236f52e299)), closes [#14](https://github.com/cardano-foundation/cardano-balance-transaction/issues/14)
* nixfmt formatting in project.nix ([3b3c3bc](https://github.com/cardano-foundation/cardano-balance-transaction/commit/3b3c3bc277d26b6363db1db0da995c56526cfeb1))
* re-enable unit-tests flake output ([911c932](https://github.com/cardano-foundation/cardano-balance-transaction/commit/911c932ac40abed24fb6f03141d3cb4d91a032f0))
* run tests in CI instead of just building them ([df2f8b0](https://github.com/cardano-foundation/cardano-balance-transaction/commit/df2f8b087e301b014712ac8a0fbf139b044c25e2))


### Reverts

* restore nix build for tests ([c506c38](https://github.com/cardano-foundation/cardano-balance-transaction/commit/c506c38d57951cf2ca893cfae9b810d3b4a4dc6c))


### Code Refactoring

* remove cardano-api from library ([d89ab84](https://github.com/cardano-foundation/cardano-balance-transaction/commit/d89ab84d4336d92d7cecd2035980059e6489fe34))
* remove cardano-api from test dependencies ([79ed842](https://github.com/cardano-foundation/cardano-balance-transaction/commit/79ed842fa78135f98777b426894eae2736624482))

## 0.1.0

Initial release. Extracted from
[cardano-wallet](https://github.com/cardano-foundation/cardano-wallet)
`lib/balance-tx/`.
