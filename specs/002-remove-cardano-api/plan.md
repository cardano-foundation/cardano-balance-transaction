# Implementation Plan: Remove cardano-api dependency

**Branch**: `002-remove-cardano-api` | **Date**: 2026-04-05 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/002-remove-cardano-api/spec.md`

## Summary

Remove all `cardano-api` imports from library and test code, replacing
them with direct `cardano-ledger-*` equivalents. The migration proceeds
in incremental steps: trivial re-export swaps first, then the hard
modules (Sign.hs witness estimation via ledger lenses, Tx.hs direct
CBOR serialization), then Eras.hs public API cleanup, and finally test
generator rewrites.

## Technical Context

**Language/Version**: Haskell (GHC 9.6+, via haskell.nix)
**Primary Dependencies**: `cardano-ledger-*` (core, api, binary, conway,
dijkstra, shelley, babbage, alonzo, allegra, mary),
`cardano-coin-selection`, `ouroboros-consensus`, `cardano-crypto-*`,
`cardano-addresses`
**Storage**: N/A
**Testing**: QuickCheck property tests, HUnit, hspec
**Target Platform**: Linux (Nix)
**Project Type**: Library
**Performance Goals**: No performance regression in balanced transaction
output
**Constraints**: Byte-identical CBOR serialization before/after
**Scale/Scope**: 4 library modules, 3 test modules, ~500 lines of test
generators

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Minimal Dependency Surface | ✅ DRIVING | This feature directly implements this principle |
| II. Ledger-Native Types | ✅ DRIVING | Public API will use only ledger types |
| III. Era Discipline | ✅ | `IsRecentEra` will be simplified (fewer constraints) |
| IV. Property-Based Testing | ✅ | Generators rewritten against ledger types |
| V. Behavioral Preservation | ✅ GATE | All tests must pass, CBOR must be byte-identical |
| VI. Incremental Migration | ✅ GATE | Each commit must compile and pass tests |

No violations. Two principles act as gates throughout.

## Project Structure

### Documentation (this feature)

```text
specs/002-remove-cardano-api/
├── plan.md
├── research.md
├── data-model.md
├── checklists/
│   └── requirements.md
└── tasks.md
```

### Source Code (repository root)

```text
lib/
├── Cardano/Balance/Tx/
│   ├── Eras.hs          # CardanoApiEra removal, IsRecentEra simplification
│   ├── Gen.hs           # Trivial: swap Cardano.Api.Ledger → ledger imports
│   ├── Sign.hs          # Hard: rewrite estimateKeyWitnessCounts with ledger lenses
│   └── Tx.hs            # Medium: direct CBOR, remove toCardanoApiTx bridge

test/spec/
├── Cardano/
│   ├── Api/
│   │   └── Gen.hs       # Hard: rewrite ~500 lines of generators
│   ├── Balance/Tx/
│   │   └── BalanceSpec.hs  # Medium: adapt to rewritten generators
│   └── Ledger/Credential/
│       └── Safe.hs      # Trivial: swap SlotNo import

cardano-balance-tx.cabal  # Remove cardano-api from build-depends
```

**Structure Decision**: Existing Haskell library layout. No new modules
needed — all changes are in-place replacements within existing files.
