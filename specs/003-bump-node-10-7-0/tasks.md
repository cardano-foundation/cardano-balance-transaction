# Tasks: Bump cardano-node to 10.7.0

**Input**: Design documents from `/specs/003-bump-node-10-7-0/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

## Phase 1: Library Compatibility

- [ ] T001 [US1] Remove stale `Timelock` imports and constraints in
  `lib/Cardano/Balance/Tx/Eras.hs`,
  `lib/Cardano/Balance/Tx/Primitive/Convert.hs`,
  `lib/Cardano/Balance/Tx/Sign.hs`, and
  `lib/Cardano/Balance/Tx/Balance.hs`.
- [ ] T002 [US1] Adapt native script conversion and witness estimation
  to Conway and Dijkstra concrete script types using `RecentEra`-guided
  branching.
- [ ] T003 [US1] Rebuild `lib:cardano-balance-tx` and fix any cascading
  ledger-core `1.19` API changes that surface.

## Phase 2: Test Compatibility

- [ ] T004 [US2] Remove imports that depend on deleted ledger `testlib`
  sublibraries in `test/spec/Cardano/Balance/Tx/TxSpec.hs` and
  `test/spec/Cardano/Balance/Tx/BalanceSpec.hs`.
- [ ] T005 [US2] Add the narrowest local or supported replacements for
  required ledger `Arbitrary` instances and generators.
- [ ] T006 [US2] Rebuild `test:unit` and resolve any test-only ledger
  API changes.

## Phase 3: Integration

- [ ] T007 [US3] Run `nix develop --quiet -c cabal build all -O0` and
  confirm the branch builds cleanly.
