# Data Model: Bump cardano-node to 10.7.0

## Recent Era Native Script Support

- **Conway native script**
  - Existing representation remains timelock-compatible.
  - Used where recent-era helpers still produce ledger-native scripts.
- **Dijkstra native script**
  - Uses `DijkstraNativeScript`.
  - Requires explicit era-aware conversion instead of a shared `Timelock`
    equality constraint.

## Test Generator Support

- **Ledger Arbitrary source**
  - Existing tests rely on upstream orphan `Arbitrary` instances.
  - The source package changed, so the project may need local orphan
    imports or replacement generators for affected ledger types.

## Migration State

- **Current bump baseline**
  - Dependency bounds and flake inputs already point to `10.7.0`.
  - Recent-era constraints were partially updated in `Eras.hs`.
- **Remaining state**
  - Library build still trips over stale imports/constraints.
  - Test build is expected to fail on removed `testlib` sublibraries
    after library compatibility is restored.
