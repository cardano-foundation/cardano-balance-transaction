# Getting Started

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- Or: GHC 9.12.2, cabal-install, and the Cardano Haskell package
  repository ([CHaP](https://github.com/IntersectMBO/cardano-haskell-packages))

## Building with Nix (recommended)

```bash
# Clone the repository
git clone https://github.com/cardano-foundation/cardano-balance-transaction.git
cd cardano-balance-transaction

# Enter the development shell
nix develop

# Build the library and tests through the same flake path used by CI
just build

# Run the test suite
just unit
```

`just build` runs `nix build .#lib .#unit-tests` and `just unit` runs
`nix run .#unit-tests`; both match the CI workflow. Inside `nix develop`
you can also drive cabal directly:

```bash
cabal build lib:cardano-balance-tx -O0
cabal test unit -O0
```

## Building without Nix

You need GHC 9.12.2 and access to CHaP. Add to your
`cabal.project` (or use the one in this repository):

```
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
```

Then:

```bash
cabal update
cabal build lib:cardano-balance-tx -O0
cabal test unit -O0
```

## Test suite

The test suite combines QuickCheck property tests with golden tests
ported from `cardano-wallet` (including serialization round-trips across
the Babbage, Conway, and Dijkstra golden fixtures under `test/data/`).
Run it with:

```bash
just unit
```

Filter to a subset by passing a match pattern:

```bash
just unit "Surplus"
```

Or, inside `nix develop`, with verbose cabal output:

```bash
cabal test unit -O0 --test-show-details=direct
```

## Documentation site

Build or serve the MkDocs documentation locally:

```bash
just docs-serve   # serve at http://127.0.0.1:8000
just docs-build   # mkdocs build --strict
```

`just docs-serve` runs `mkdocs serve` and `just docs-build` runs
`mkdocs build --strict`. The MkDocs toolchain is provided by the
`github:paolino/dev-assets?dir=mkdocs` flake (see the `docs` job in
`.github/workflows/ci.yml`).
