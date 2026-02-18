# Changelog

## 1.0.0 (2026-02-16)


### Features

* copy balance-tx source with namespace rename ([00a13d1](https://github.com/cardano-foundation/cardano-balance-transaction/commit/00a13d1e3b1d3805cf94872f4a7a24b1a46ef0ff))
* remove all wallet dependencies from library and test suite ([4f4d1d7](https://github.com/cardano-foundation/cardano-balance-transaction/commit/4f4d1d76c8ab976159368440ec8ae7bb6af37ba8))
* replace wallet-primitive types with standalone Primitive module ([bcff708](https://github.com/cardano-foundation/cardano-balance-transaction/commit/bcff708ae0e292a7e5f86494726ed2c3a7768959))
* repository skeleton with extraction plan ([9ff2f23](https://github.com/cardano-foundation/cardano-balance-transaction/commit/9ff2f23d75119b1dfbcf262d820a69056485b563))


### Bug Fixes

* add CHaP flake input to avoid DNS resolution in Nix sandbox ([ec74279](https://github.com/cardano-foundation/cardano-balance-transaction/commit/ec7427959a97ef78e709d395e8118ffae850e73b))
* enable -Werror and fix all warnings ([a069da8](https://github.com/cardano-foundation/cardano-balance-transaction/commit/a069da87ad0ced93f1f9e66061f1e9228a881f59))
* hardcode test data path so nix run works ([27c7600](https://github.com/cardano-foundation/cardano-balance-transaction/commit/27c7600a0f0ae60dd30f95308446b354be5010e5))
* nixfmt formatting in project.nix ([3b3c3bc](https://github.com/cardano-foundation/cardano-balance-transaction/commit/3b3c3bc277d26b6363db1db0da995c56526cfeb1))
* re-enable unit-tests flake output ([911c932](https://github.com/cardano-foundation/cardano-balance-transaction/commit/911c932ac40abed24fb6f03141d3cb4d91a032f0))
* run tests in CI instead of just building them ([df2f8b0](https://github.com/cardano-foundation/cardano-balance-transaction/commit/df2f8b087e301b014712ac8a0fbf139b044c25e2))


### Reverts

* restore nix build for tests ([c506c38](https://github.com/cardano-foundation/cardano-balance-transaction/commit/c506c38d57951cf2ca893cfae9b810d3b4a4dc6c))

## 0.1.0

Initial release. Extracted from
[cardano-wallet](https://github.com/cardano-foundation/cardano-wallet)
`lib/balance-tx/`.
