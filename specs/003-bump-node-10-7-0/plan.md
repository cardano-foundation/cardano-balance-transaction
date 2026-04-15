# Implementation Plan: Bump cardano-node to 10.7.0

**Branch**: `003-bump-node-10-7-0` | **Date**: 2026-04-15 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-bump-node-10-7-0/spec.md`

## Summary

Finish the existing dependency bump by addressing the remaining source
compatibility breaks introduced by ledger `1.19`. The work proceeds in
three steps: stabilize the recent-era abstractions and native script
helpers, replace removed upstream test generator dependencies, and then
resolve any remaining cascading API changes until `cabal build all -O0`
is clean.

## Technical Context

**Language/Version**: Haskell with GHC 9.12.2 in the project flake
**Primary Dependencies**: `cardano-ledger-*`, `cardano-addresses`,
`cardano-coin-selection`, `QuickCheck`
**Storage**: N/A
**Testing**: `cabal build` for library and unit test targets
**Target Platform**: Linux via Nix dev shell
**Project Type**: Library
**Performance Goals**: No runtime behavior changes; compile-time
compatibility only
**Constraints**: Preserve the two-recent-era model and keep changes
focused on compatibility with ledger `1.19`
**Scale/Scope**: 3 library modules, test imports/generators, and any
follow-on ledger-core API fixes surfaced by the build

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Minimal Dependency Surface | ✅ | Removed testlib packages must not be replaced with broader dependencies |
| II. Ledger-Native Types | ✅ | Compatibility work stays on ledger-native APIs |
| III. Era Discipline | ✅ GATE | Era differences must remain localized to `RecentEra`-based branching |
| IV. Property-Based Testing | ✅ GATE | Test generators must remain self-contained or use supported ledger modules |
| V. Behavioral Preservation | ✅ | No intended behavior change beyond compilation compatibility |
| VI. Incremental Migration | ✅ GATE | Each fix should keep the project moving toward a buildable state |

## Project Structure

### Documentation (this feature)

```text
specs/003-bump-node-10-7-0/
├── plan.md
├── research.md
├── data-model.md
└── tasks.md
```

### Source Code (repository root)

```text
lib/Cardano/Balance/Tx/
├── Eras.hs
├── Primitive/Convert.hs
├── Sign.hs
└── Balance.hs

test/spec/Cardano/Balance/Tx/
├── BalanceSpec.hs
└── TxSpec.hs

cardano-balance-tx.cabal
```

**Structure Decision**: Apply compatibility fixes in place. No new
production modules are expected; test support may gain small local
orphan-instance helpers if upstream generators no longer exist.
