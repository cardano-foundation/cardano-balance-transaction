# WIP: Issue #17 - Promote DijkstraEra to RecentEra

## Status
Blocked — CHaP repo branch hasn't published node 10.7 ecosystem yet

## What's done
- [x] Speckit initialized, constitution written
- [x] Feature specified (`specs/001-dijkstra-recent-era/spec.md`)
- [x] Plan written (`specs/001-dijkstra-recent-era/plan.md`)
- [x] Research documented (`specs/001-dijkstra-recent-era/research.md`)
- [x] Tasks generated (`specs/001-dijkstra-recent-era/tasks.md`)
- [x] Issue #17 created and added to planner
- [x] Issue #18 created (CHaP rollback tracking)
- [ ] Dependency bump (T001-T006) — BLOCKED

## Blocker
cardano-api 10.26.0.0 requires the full node 10.7 ecosystem:
- ouroboros-consensus 2.0 (with sublibraries)
- ouroboros-network 1.1 (with `api` sublibrary)
- io-classes with `mtl` sublibrary

These are in CHaP `_sources` on `main` but NOT yet on the `repo` branch
(the actual cabal package index). The entire set must land together.

## Changes made to build files
- `cabal.project`: bumped index-states, added constraints, allow-newer
- `cardano-balance-tx.cabal`: bumped dep bounds, switched to sublibraries
- `flake.nix`: bumped indexState
- `flake.lock`: updated CHaP + haskellNix inputs
- `nix/project.nix`: disabled hoogle (GHC 9.12.2 panic on ouroboros-network haddock)

## When unblocked
1. Check CHaP repo branch for cardano-api 10.26 availability
2. Remove source-repository-package for cardano-api
3. Verify full build: `nix develop --quiet -c cabal build lib:cardano-balance-tx -O0`
4. Enable tests (Phase 2 tasks T007-T011)
5. Polish (Phase 3 tasks T012-T014)

## Key files modified
- cabal.project
- cardano-balance-tx.cabal
- flake.nix
- flake.lock
- nix/project.nix
