# Research: Bump cardano-node to 10.7.0

## Decisions

### Decision: Model native script differences through recent-era branching

- **Why**: Conway and Dijkstra no longer share the same concrete native
  script type. Dijkstra uses `DijkstraNativeScript`, so the previous
  blanket `Timelock` equality constraint is no longer valid.
- **Alternative considered**: Push a more complex typeclass abstraction
  across all script conversion helpers. Rejected because the project
  already centralizes era-specific behavior through `RecentEra`, and the
  constitution prefers localized era branching.

### Decision: Replace removed ledger testlib dependencies locally

- **Why**: The removed `testlib` sublibraries are compatibility debt,
  not feature work. Local test support is the narrowest fix that
  satisfies the project constitution.
- **Alternative considered**: Add new upstream-only dependencies or pin
  older ledger packages. Rejected because it increases dependency
  surface and undermines the version bump.

### Decision: Use build-driven migration for cascading ledger-core changes

- **Why**: The handover already identifies likely failure areas, but the
  full set of required edits depends on actual compiler errors after
  each compatibility fix.
- **Alternative considered**: Preemptively rewrite broad areas of code.
  Rejected because it would create a larger, harder-to-review diff.
