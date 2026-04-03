# Research: Promote DijkstraEra to RecentEra

## Decision 1: Blocker Resolution

**Decision**: The cardano-api DijkstraEra runtime blocker (issue #12) is
resolved.

**Rationale**: cardano-api PR #1050 ("Node 10.7 integration") merged on
2026-03-18. Version 10.26.0.0 was published to CHaP on 2026-04-02,
containing the commit "Add DijkstraEra to transaction creation".

**Alternatives considered**: None — this was the only path forward.

## Decision 2: No Code Changes Needed in Library

**Decision**: The library code is already complete for Dijkstra. Only
dependency bumps and test enablement are required.

**Rationale**: The codebase was refactored in commits `cce0014`,
`3bc9094`, `799a1c8`, and `7e01487` to:
- Add `RecentEraDijkstra` to the `RecentEra` GADT
- Add `IsRecentEra Dijkstra` instance
- Set `LatestLedgerEra = DijkstraEra`
- Update all library modules (Balance, Sign, Redeemers, etc.)
- Update certificate types for Dijkstra
- Add pending Dijkstra tests

The `RecentEraConstraints` does NOT contain `Core.Tx era ~ AlonzoTx era`
(which would have been false for Dijkstra). All transaction access uses
lenses (`bodyTxL`, `witsTxL`, etc.) which work through newtypes.

**Alternatives considered**: Full constraint rewrite was considered but
found unnecessary after code analysis.

## Decision 3: cardano-api Version Target

**Decision**: Bump to `>=10.26.0.0 && <10.27`.

**Rationale**: 10.26.0.0 is the first version with full DijkstraEra
runtime support. It was released to CHaP on 2026-04-02 (CHaP commit
`229dcd4`).

**Alternatives considered**:
- 10.25.0.0: Does not contain the Dijkstra transaction creation commit
- Waiting for 10.27+: No reason to wait; 10.26 has what we need

## Decision 4: CHaP Index-State

**Decision**: Advance to `2026-04-03T00:00:00Z`.

**Rationale**: The current index-state (`2026-02-09T17:36:58Z`) predates
cardano-api 10.26.0.0 (published 2026-04-02T17:15:57Z). Must advance
past that timestamp.

**Alternatives considered**: Exact timestamp `2026-04-02T17:16:00Z` would
work but a round date is cleaner and captures any same-day fixes.
