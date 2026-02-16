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

# Build the library
cabal build lib:cardano-balance-tx -O0

# Run the test suite
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

The test suite contains 268 property tests and golden tests ported
from `cardano-wallet`. Run with:

```bash
cabal test unit -O0
```

To run with verbose output:

```bash
cabal test unit -O0 --test-show-details=direct
```

## Documentation site

Build the MkDocs documentation locally:

```bash
nix develop
mkdocs serve
```

Then open <http://127.0.0.1:8000>.
