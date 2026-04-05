# Tasks: Remove cardano-api dependency

**Input**: Design documents from `/specs/002-remove-cardano-api/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Exact file paths included in all descriptions

## Phase 1: Setup

**Purpose**: Golden test baseline and verification infrastructure

- [x] T001 Add CBOR golden test capturing current serialization output for Conway and Dijkstra transactions in test/spec/Cardano/Balance/Tx/SerializationGoldenSpec.hs
- [ ] T002 [P] Add a CI check script that greps for `Cardano.Api` imports and fails if any are found, in scripts/check-no-cardano-api.sh

**Checkpoint**: Golden baseline captured, regression detection in place

---

## Phase 2: Foundational (Trivial Import Swaps)

**Purpose**: Remove cardano-api from files where it's only used for re-exported ledger types. Each task is independently compilable.

- [x] T003 Replace `Cardano.Api.Ledger` imports with direct ledger imports in lib/Cardano/Balance/Tx/Gen.hs (`Coin` → `Cardano.Ledger.Coin`, `CostModels` → `Cardano.Ledger.Alonzo.Scripts`, `PParams`/`PParamsHKD`/`UpgradeConwayPParams` → `Cardano.Ledger.Core`/`Cardano.Ledger.Conway.PParams`)
- [x] T004 [P] Replace `Cardano.Api (SlotNo)` import with `Cardano.Slotting.Slot` in test/spec/Cardano/Ledger/Credential/Safe.hs. Add `cardano-slotting` to test build-depends if not already present.

**Checkpoint**: Gen.hs and Safe.hs have zero cardano-api imports. Library and tests compile and pass.

---

## Phase 3: User Story 1 — Build without cardano-api (Priority: P1) 🎯 MVP

**Goal**: All library modules compile without cardano-api. The cabal library stanza has no cardano-api in build-depends.

**Independent Test**: `cabal build lib:cardano-balance-tx` succeeds after removing cardano-api from library build-depends.

### Implementation for User Story 1

- [x] T005 [US1] Rewrite `estimateKeyWitnessCounts` in lib/Cardano/Balance/Tx/Sign.hs: replace `CardanoApi.getTxBodyContent` + pattern matching with ledger lenses (`bodyTxL`, `inputsTxBodyL`, `collateralInputsTxBodyL`, `reqSignerHashesTxBodyL`, `withdrawalsTxBodyL`, `certsTxBodyL`). Remove `txUpdateProposal` handling. Replace `Cardano.Api.Experimental.Certificate` usage with direct `Cardano.Ledger.Conway.TxCert`/`Cardano.Ledger.Dijkstra.TxCert`. Remove all `Cardano.Api` imports from the file.
- [x] T006 [US1] Rewrite serialization in lib/Cardano/Balance/Tx/Tx.hs: replace `CardanoApi.serialiseToCBOR`/`CardanoApi.deserialiseFromCBOR` with `Cardano.Ledger.Binary.serialize`/`decodeFull`. Remove `toCardanoApiTx` and `fromCardanoApiTx`. Replace `AnyCardanoEra` usage in `toRecentEraGADT` with a local error type or string. Remove all `Cardano.Api` imports from the file.
- [x] T007 [US1] Simplify `IsRecentEra` in lib/Cardano/Balance/Tx/Eras.hs: remove `CardanoApiEra` type family, drop `IsShelleyBasedEra` and `ShelleyLedgerEra` from `IsRecentEra` superclass constraints. Remove `cardanoEraFromRecentEra`, `shelleyBasedEraFromRecentEra`, `toAnyCardanoEra`, `fromAnyCardanoEra`. Replace `MaybeInRecentEra` non-recent constructors that reference `AnyCardanoEra` with a simple tag. Remove all `Cardano.Api` imports from the file.
- [x] T008 [US1] Remove `cardano-api` from library build-depends in cardano-balance-tx.cabal. Verify `cabal build lib:cardano-balance-tx` compiles.

**Checkpoint**: Library is cardano-api-free. `cabal build lib:cardano-balance-tx` passes. Tests may not compile yet (test generators still use cardano-api).

---

## Phase 4: User Story 2 — Ledger-native public API (Priority: P2)

**Goal**: No cardano-api types in any exported module signature.

**Independent Test**: Generate Haddock docs and verify no cross-references to `Cardano.Api.*` modules.

### Implementation for User Story 2

- [ ] T009 [US2] Audit all exposed modules listed in cardano-balance-tx.cabal for any residual cardano-api type references in public signatures. Document findings in specs/002-remove-cardano-api/api-audit.md.
- [ ] T010 [US2] Fix any residual cardano-api types found in T009. Update Haddock comments that reference cardano-api types.
- [ ] T011 [US2] Build Haddock documentation (`cabal haddock`) and verify no broken cross-references to cardano-api modules.

**Checkpoint**: Public API is fully ledger-native. Haddock builds clean.

---

## Phase 5: User Story 3 — Self-contained test generators (Priority: P3)

**Goal**: Test suite compiles and passes with cardano-api fully removed from test build-depends.

**Independent Test**: `cabal test` succeeds after removing cardano-api from test build-depends.

### Implementation for User Story 3

- [ ] T012 [US3] Delete test/spec/Cardano/Api/Gen.hs (orphaned — nothing imports it after T013). Remove `Cardano.Api.Gen` from cabal other-modules.
- [x] T013 [US3] Replace all `Cardano.Api`/`Cardano.Api.Gen` usage in test/spec/Cardano/Balance/Tx/BalanceSpec.hs with ledger-native generators. All generators rewritten inline; Gen.hs no longer imported.
- [ ] T014 [US3] Remove `cardano-api` from test build-depends in cardano-balance-tx.cabal. Verify `cabal test` compiles and all tests pass.
- [ ] T015 [US3] Run the CBOR golden test from T001 to confirm byte-identical serialization after migration.

**Checkpoint**: Full test suite passes. Zero cardano-api imports anywhere. Golden tests confirm behavioral preservation.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [ ] T016 Run the no-cardano-api grep check from T002 and verify zero matches.
- [ ] T017 [P] Verify `cabal build --dry-run` dependency plan does not include cardano-api.
- [ ] T018 [P] Open downstream migration ticket(s) per FR-008 with detailed instructions (removed types, renamed imports, replacement patterns).
- [ ] T019 Update README.md to remove any references to cardano-api compatibility.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: No dependencies — can run in parallel with Phase 1
- **US1 (Phase 3)**: Depends on Phase 2 completion (trivial swaps done)
- **US2 (Phase 4)**: Depends on Phase 3 completion (library is api-free)
- **US3 (Phase 5)**: Depends on Phase 3 completion (library compiles without cardano-api)
- **Polish (Phase 6)**: Depends on Phase 5 completion

### Within User Story 1

- T005, T006, T007 can be developed in parallel (different files) but must all be done before T008
- T008 is the integration point — removing the cabal dep verifies everything compiles together

### Within User Story 3

- T012 must complete before T013 (module rename)
- T013 must complete before T014 (cabal dep removal)
- T015 runs after T014

### Parallel Opportunities

```text
Phase 1+2: T001, T002, T003, T004 — all independent files
Phase 3:   T005, T006, T007 — different lib files, parallel dev
Phase 5:   T012 alone (large), then T013, then T014+T015
Phase 6:   T016, T017, T018 — all independent
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. T001, T002 — golden baseline
2. T003, T004 — trivial swaps
3. T005, T006, T007 — hard module rewrites
4. T008 — remove cabal dep, verify lib compiles
5. **STOP and VALIDATE**: Library is cardano-api-free

### Incremental Delivery

1. Phases 1+2 → Foundation ready
2. Phase 3 → Library independent (MVP)
3. Phase 4 → Public API clean
4. Phase 5 → Tests independent
5. Phase 6 → Downstream notified, fully verified
