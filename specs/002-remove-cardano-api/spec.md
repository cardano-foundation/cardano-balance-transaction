# Feature Specification: Remove cardano-api dependency

**Feature Branch**: `002-remove-cardano-api`
**Created**: 2026-04-05
**Status**: Draft
**Input**: User description: "Remove cardano-api dependency (issue #9)"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Build without cardano-api (Priority: P1)

A library consumer adds `cardano-balance-tx` to their project and builds
successfully without `cardano-api` appearing anywhere in their transitive
dependency closure. Their build is faster and their dependency footprint
is smaller.

**Why this priority**: This is the core deliverable. If the library
still pulls in `cardano-api` transitively, nothing else matters.

**Independent Test**: Build the library with `cardano-api` removed from
the cabal file. All library modules compile. All existing tests pass.

**Acceptance Scenarios**:

1. **Given** a project depending only on `cardano-balance-tx`, **When**
   the dependency is resolved, **Then** `cardano-api` does not appear in
   the dependency plan.
2. **Given** the library cabal file, **When** `cardano-api` is removed
   from `build-depends`, **Then** all library modules compile without
   errors.
3. **Given** the full test suite, **When** tests are built and executed
   after the migration, **Then** all tests pass with identical results.

---

### User Story 2 - Ledger-native public API (Priority: P2)

A library consumer inspects the public API and finds only
`cardano-ledger-*` types in all exported signatures. There are no
`CardanoApi.Tx`, `AnyCardanoEra`, or `CardanoApiEra` type family
references. The consumer works directly with ledger types throughout.

**Why this priority**: Removing the dependency is insufficient if the
public API still exposes `cardano-api` types through re-exports or type
families. The API must be ledger-native for the library to be truly
standalone.

**Independent Test**: Review all exported module signatures. No type
from `Cardano.Api.*` appears in any public type signature or constraint.

**Acceptance Scenarios**:

1. **Given** the `Eras` module, **When** a consumer uses `IsRecentEra`,
   **Then** the constraint does not mention any `cardano-api` type class
   or type family.
2. **Given** the `Tx` module, **When** a consumer serializes or
   deserializes a transaction, **Then** only ledger types are involved —
   no `CardanoApi.Tx` wrapper.
3. **Given** any exposed module, **When** Haddock documentation is
   generated, **Then** no cross-references point to `cardano-api`
   modules.

---

### User Story 3 - Self-contained test generators (Priority: P3)

A contributor adding new tests writes generators using only ledger types
and the project's own generator modules. No generator depends on
`cardano-api` generator utilities.

**Why this priority**: Test generators are the largest consumer of
`cardano-api` by code volume, but they don't affect downstream users.
This can be done last without blocking the library's independence.

**Independent Test**: Build and run the test suite with `cardano-api`
fully absent from both library and test `build-depends`.

**Acceptance Scenarios**:

1. **Given** the test cabal stanza, **When** `cardano-api` is removed
   from test `build-depends`, **Then** all test modules compile.
2. **Given** the `Cardano.Api.Gen` test module, **When** it is rewritten
   to use ledger types, **Then** all generators produce valid values and
   all property tests pass.

---

### Edge Cases

- What if a `cardano-api` type has no direct ledger equivalent (e.g.
  `TxBodyContent` record)? The equivalent functionality must be
  reconstructed using ledger lenses.
- What if `cardano-api` serialization produces different CBOR bytes than
  direct ledger serialization? Behavioral equivalence must be verified
  with round-trip tests.
- What if `ouroboros-consensus` (used for `StandardCrypto`) itself
  depends on `cardano-api`? The `StandardCrypto` import path must be
  verified to not reintroduce the dependency.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The library MUST compile and pass all tests with
  `cardano-api` completely absent from both library and test
  `build-depends`.
- **FR-002**: All public type signatures MUST use only `cardano-ledger-*`
  types. No `Cardano.Api.*` types may appear in exported APIs.
- **FR-003**: The `IsRecentEra` type class MUST NOT reference any
  `cardano-api` type class (`IsShelleyBasedEra`) or type family
  (`ShelleyLedgerEra`, `CardanoApiEra`).
- **FR-004**: Transaction serialization and deserialization MUST produce
  byte-identical output to the current implementation.
- **FR-005**: Key witness estimation MUST produce identical counts to the
  current implementation for all inputs.
- **FR-006**: Test generators MUST produce values with the same
  distribution characteristics as the current generators.
- **FR-007**: Each migration increment MUST leave the project in a
  compilable, all-tests-passing state (per Constitution Principle VI).
- **FR-008**: A migration ticket MUST be opened in each downstream
  consumer repository with detailed instructions on how to adapt to
  the new ledger-native API (removed types, renamed imports,
  replacement patterns).

### Key Entities

- **RecentEra**: The library's era GADT — currently coupled to
  `cardano-api` via `CardanoApiEra` type family and `IsShelleyBasedEra`
  constraint. Must become self-contained.
- **Tx**: The transaction type — currently wrapped/unwrapped through
  `CardanoApi.ShelleyTx`. Must use ledger `Tx` directly.
- **KeyWitnessCounts**: Output of witness estimation — currently
  computed by deconstructing `CardanoApi.TxBodyContent`. Must use ledger
  lenses.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Zero imports from `Cardano.Api`, `Cardano.Api.Ledger`, or
  `Cardano.Api.Experimental` across all library and test source files.
- **SC-002**: `cardano-api` does not appear in the resolved dependency
  plan when building `cardano-balance-tx`.
- **SC-003**: 100% of existing tests pass without modification to test
  assertions (modifications to test infrastructure/generators are
  expected).
- **SC-004**: CBOR serialization round-trips produce identical bytes
  before and after migration (verified by golden tests or comparison).
- **SC-005**: The migration is delivered as a series of individually
  compilable, test-passing increments — not a single large changeset.

## Assumptions

- All types currently imported via `Cardano.Api.Ledger` are available
  directly from `cardano-ledger-*` packages already in the dependency
  list.
- `StandardCrypto` can be imported from `cardano-protocol-tpraos`
  (`Cardano.Protocol.Crypto`) instead of `ouroboros-consensus`.
- Ledger lenses provide equivalent access to all transaction body fields
  currently accessed via `CardanoApi.getTxBodyContent`.
- The `cardano-ledger-binary` package provides equivalent CBOR
  serialization to `CardanoApi.serialiseToCBOR` for ledger `Tx` values.
