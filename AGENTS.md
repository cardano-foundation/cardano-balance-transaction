# Repository Agent Guide

## What this repo is

`cardano-balance-tx` is a standalone Haskell library that balances
Cardano transactions: given a partial transaction (user-specified
outputs, but incomplete inputs and fees), a UTxO set, and protocol
parameters, its single public entry point `balanceTx` produces a fully
balanced, ready-to-sign transaction. It was extracted from
`cardano-wallet`'s `lib/balance-tx/` and works directly with
`cardano-ledger` types, targeting the two most recent eras (Conway and
Dijkstra). There is **no executable** — it is consumed as a library.

## How to work here

All recipes route through the flake, matching CI
(`.github/workflows/ci.yml`):

- Enter the dev shell: `nix develop`
- Build (lib + tests): `just build`  (`nix build .#lib .#unit-tests`)
- Run tests: `just unit`  (`nix run .#unit-tests`); filter with
  `just unit "<match>"`
- Full local CI: `just CI`  (build + tests + fourmolu + hlint + nixfmt)
- Format: `just format`  (fourmolu, cabal-fmt, nixfmt)
- Lint: `just hlint`
- Docs: `just docs-serve` / `just docs-build` (`mkdocs build --strict`)

Inside `nix develop` you can also use cabal directly with `-O0`, e.g.
`cabal build lib:cardano-balance-tx -O0` and `cabal test unit -O0`.

Source lives in `lib/Cardano/Balance/Tx/`; tests in `test/spec/` with
fixtures in `test/data/`. Dependency pins are in `cabal.project`; the
package metadata is in `cardano-balance-tx.cabal`.

## Skills

Activatable procedures live under `skills/`. Load the one whose
description matches your task:

- `skills/cardano-balance-transaction-guide/` — repository map, build/test
  commands, how to navigate the code, how to use `balanceTx`, and where
  answers to common questions about this repo live.

## Documentation

Human-facing docs are in `docs/` and published at
<https://cardano-foundation.github.io/cardano-balance-transaction/>. The
`README.md` is the entry point.
