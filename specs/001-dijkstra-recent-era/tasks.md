# Tasks: Promote DijkstraEra to RecentEra

**Input**: Design documents from `/specs/001-dijkstra-recent-era/`
**Prerequisites**: plan.md, spec.md, research.md

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to

---

## Phase 1: Dependency Bump

**Purpose**: Unblock Dijkstra runtime support by bumping cardano-api

- [ ] T001 Advance CHaP index-state in `cabal.project` from `2026-02-09T17:36:58Z` to `2026-04-03T00:00:00Z`
- [ ] T002 Advance Hackage index-state in `cabal.project` from `2026-01-29T00:00:00Z` to `2026-04-03T00:00:00Z`
- [ ] T003 Bump cardano-api version bound in `cardano-balance-tx.cabal` from `>=10.23.0.0 && <10.24` to `>=10.26.0.0 && <10.27`
- [ ] T004 Run `nix flake update` to refresh `flake.lock` with new CHaP index
- [ ] T005 Run `cabal build lib:cardano-balance-tx -O0` and fix any transitive dependency breakage
- [ ] T006 Update Nix `indexState` in `flake.nix` (line 38) from `"2025-10-01T00:00:00Z"` to `"2026-04-03T00:00:00Z"`

**Checkpoint**: Library builds with cardano-api 10.26.0.0

---

## Phase 2: Enable Dijkstra Tests (US1 + US2 + US3)

**Goal**: Remove all pending guards and enable Dijkstra test execution

**Independent Test**: `cabal test unit -O0` passes with Dijkstra tests running

- [ ] T007 [US1] Replace `pendingDijkstra` block with `tests RecentEraDijkstra` in `forAllRecentEras` in `test/spec/Cardano/Balance/Tx/BalanceSpec.hs` (lines 451-456)
- [ ] T008 [US1] Remove the `pendingDijkstra` function definition in `test/spec/Cardano/Balance/Tx/BalanceSpec.hs` (lines 2666-2673)
- [ ] T009 [US2] Restore `boundedEnumLaws` in `test/spec/Cardano/Balance/Tx/TxSpec.hs` — replace the pending `it` block (lines 74-79) with `boundedEnumLaws` in the `testLawsMany` list
- [ ] T010 [US1] Generate golden test data directory `test/data/balanceTx/dijkstra/` by running the golden tests in update mode (or copying from `conway/` and regenerating)
- [ ] T011 [US1] Run `cabal test unit -O0` and fix any Dijkstra test failures

**Checkpoint**: All tests pass including Dijkstra era

---

## Phase 3: Polish & Verification

**Purpose**: Clean up, verify CI, close issue

- [ ] T012 Update README.md supported eras section to list "Conway, Dijkstra" instead of "Babbage, Conway"
- [ ] T013 Run `just ci` locally to verify formatting, linting, and full test suite
- [ ] T014 Close issue #12 (cardano-api DijkstraEra runtime support) with a comment linking to this PR

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1** (Dependency Bump): No dependencies — start immediately
- **Phase 2** (Enable Tests): Depends on Phase 1 (library must build first)
- **Phase 3** (Polish): Depends on Phase 2 (tests must pass first)

### Within Phase 1

Sequential: T001 → T002 → T003 → T004 → T005 → T006 (each depends on prior)

### Within Phase 2

- T007 + T008 can be done together (same file, related changes)
- T009 is independent (different file) — [P] with T007/T008
- T010 depends on T007 (golden tests need Dijkstra enabled to generate)
- T011 depends on all of T007-T010

### Parallel Opportunities

- T007/T008 and T009 can be done in parallel (different test files)
- T012 can be done in parallel with any Phase 2 task

---

## Implementation Strategy

### MVP (Phase 1 + Phase 2)

1. Bump dependencies (Phase 1)
2. Enable tests (Phase 2)
3. **VALIDATE**: All 268+ existing tests pass, plus new Dijkstra tests
4. Push and create PR

### Follow-up (separate PR in cardano-wallet)

1. Update cardano-wallet's `source-repository-package` pin to new commit
2. Replace 10 Dijkstra `error` stubs in wallet with working implementations
3. Update wallet's `cardano-balance-tx` `Eras.hs` re-exports

---

## Notes

- Total tasks: 14
- Phase 1 (Dependency Bump): 6 tasks
- Phase 2 (Enable Tests): 5 tasks
- Phase 3 (Polish): 3 tasks
- The library code itself needs NO changes — only build config and test enablement
- Golden test data for Dijkstra may need manual review if serialization differs from Conway
