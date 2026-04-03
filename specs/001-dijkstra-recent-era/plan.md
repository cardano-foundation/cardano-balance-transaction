# Implementation Plan: Promote DijkstraEra to RecentEra

**Branch**: `feat/dijkstra-recent-era` | **Date**: 2026-04-03 | **Spec**: [spec.md](spec.md)
**Issue**: cardano-foundation/cardano-balance-transaction#17

## Summary

Enable full Dijkstra-era transaction balancing by bumping cardano-api to
10.26.0.0 (which now has DijkstraEra runtime support) and unblocking the
pending Dijkstra tests. The library code is already fully updated — this
is a dependency bump + test enablement, not a code rewrite.

## Technical Context

**Language/Version**: Haskell / GHC 9.12.2
**Primary Dependencies**: cardano-api (10.23 → 10.26), cardano-ledger-*, ouroboros-consensus
**Testing**: cabal test (QuickCheck property tests, 268+ examples)
**Project Type**: Library
**Constraints**: Must maintain backward compatibility with Conway era

## Current State (already done)

The following Dijkstra work has **already been merged to main**:

- `RecentEra` GADT: `RecentEraDijkstra` constructor exists
- `IsRecentEra Dijkstra` instance exists
- `LatestLedgerEra = DijkstraEra`
- `MaybeInRecentEra`: `InRecentEraDijkstra` constructor exists
- `CardanoApiEra Dijkstra = CardanoApi.DijkstraEra`
- All library modules updated: Balance, Sign, Redeemers, Convert, Tx,
  Gen, SizeEstimation
- `RecentEraConstraints` already works for Dijkstra (no `Core.Tx era ~
  AlonzoTx era` constraint — all code is lens-based)
- Babbage already dropped from RecentEra (now Conway + Dijkstra)
- Certificate types handled (`DijkstraTxCertDeleg`, etc.)

## Blocker Resolution

**Issue #12**: cardano-api DijkstraEra runtime support was missing.

- **cardano-api PR #1050** ("Node 10.7 integration") merged 2026-03-18
- **cardano-api 10.26.0.0** released to CHaP on 2026-04-02
- Contains: `Add DijkstraEra to transaction creation`

The blocker is **resolved**. The CHaP index-state needs advancing from
`2026-02-09` to at least `2026-04-02T17:16:00Z`.

## Constitution Check

| Principle | Status |
|-----------|--------|
| I. Ledger-Native Types | PASS — no wallet types introduced |
| II. Era-Indexed Correctness | PASS — Dijkstra already in RecentEra GADT |
| III. Extracted Library Discipline | PASS — no new wallet dependencies |
| IV. Constraint-Driven Generics | PASS — constraints already verified for Dijkstra |
| V. Test Coverage | PENDING — tests exist but are disabled |

## Implementation Phases

### Phase 1: Dependency Bump

1. **Advance CHaP index-state** in `cabal.project` from `2026-02-09`
   to `2026-04-03T00:00:00Z` (or latest available)
2. **Bump cardano-api bound** in `cardano-balance-tx.cabal` from
   `>=10.23.0.0 && <10.24` to `>=10.26.0.0 && <10.27`
3. **Update `flake.lock`** to pick up new CHaP index
4. **Fix any build breakage** from transitive dependency changes
   between cardano-api 10.23 and 10.26
5. **Verify**: `cabal build lib:cardano-balance-tx -O0` succeeds

### Phase 2: Enable Dijkstra Tests

1. **Remove `pendingDijkstra`** guard in `test/spec/Cardano/Balance/Tx/BalanceSpec.hs`
2. **Uncomment `tests RecentEraDijkstra`** in `forAllRecentEras`
3. **Restore `boundedEnumLaws`** in `test/spec/Cardano/Balance/Tx/TxSpec.hs`
4. **Generate golden test data** in `test/data/balanceTx/dijkstra/`
5. **Verify**: `cabal test unit -O0` passes with Dijkstra tests running

### Phase 3: Downstream Coordination

1. **Update cardano-wallet pin** in `cabal.project` to point to the
   new commit of cardano-balance-tx
2. **Replace wallet error stubs** with working Dijkstra implementations
   (separate PR in cardano-wallet repo)

## Project Structure

```text
specs/001-dijkstra-recent-era/
├── spec.md
├── plan.md              # This file
├── research.md
├── checklists/
│   └── requirements.md
└── tasks.md             # Next: /speckit.tasks
```

### Source Code (no new files expected)

```text
lib/Cardano/Balance/Tx/
├── Eras.hs              # Already updated
├── Balance.hs           # Already updated
├── Sign.hs              # Already updated
├── Redeemers.hs         # Already updated
├── Tx.hs                # Already updated
├── Gen.hs               # Already updated
├── SizeEstimation.hs    # Already updated
├── Primitive/Convert.hs # Already updated
└── ...

test/spec/Cardano/Balance/Tx/
├── BalanceSpec.hs       # Remove pending, enable Dijkstra tests
├── TxSpec.hs            # Restore law tests
└── Balance/
    └── TokenBundleSizeSpec.hs  # Check if Dijkstra tests needed
```

## Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| cardano-api 10.26 breaks other deps | Medium | Check transitive dep bounds, fix as needed |
| Dijkstra tests fail at runtime | Low | Library code already handles Dijkstra; failures would indicate cardano-api bugs |
| CHaP index-state advance pulls in unwanted changes | Low | Pin exact versions in constraints |
| Golden test data generation fails | Low | Generate from working Conway pattern |

## Complexity Tracking

No constitution violations — this is a dependency bump + test enablement,
not new feature code.
