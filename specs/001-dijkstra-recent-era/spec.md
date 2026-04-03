# Feature Specification: Promote DijkstraEra to RecentEra

**Feature Branch**: `001-dijkstra-recent-era`
**Created**: 2026-04-03
**Status**: Draft
**Input**: User description: "Promote DijkstraEra to a RecentEra so that cardano-balance-tx supports transaction balancing, signing, and fee estimation for the Dijkstra era alongside Conway and Babbage"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Balance a Dijkstra Transaction (Priority: P1)

A downstream consumer (cardano-wallet or any Cardano application) submits
a partial Dijkstra-era transaction with user-specified outputs but
incomplete inputs and fees. The library selects UTxO inputs, estimates
fees, constructs change outputs, and returns a fully balanced transaction
ready for signing.

**Why this priority**: This is the core function of the library. Without
balancing, no Dijkstra transaction can be constructed.

**Independent Test**: Can be tested by constructing a partial Dijkstra
transaction with known UTxO set and protocol parameters, calling
`balanceTx`, and verifying the result is a valid balanced transaction.

**Acceptance Scenarios**:

1. **Given** a partial Dijkstra-era transaction with outputs totaling
   10 ADA and a UTxO set containing 50 ADA, **When** `balanceTx` is
   called, **Then** the result is a balanced transaction with correct
   inputs, fees, and change.
2. **Given** a Dijkstra-era transaction with Plutus V4 scripts,
   **When** `balanceTx` is called, **Then** redeemer indices are
   correctly assigned and script integrity hash is computed.

---

### User Story 2 - Sign a Dijkstra Transaction (Priority: P1)

A downstream consumer takes a balanced Dijkstra transaction and signs it
with one or more signing keys. The library produces a fully witnessed
transaction.

**Why this priority**: Signing is equally essential — a balanced but
unsigned transaction cannot be submitted to the network.

**Independent Test**: Can be tested by signing a balanced Dijkstra
transaction with test keys and verifying the witness set is correct.

**Acceptance Scenarios**:

1. **Given** a balanced Dijkstra transaction, **When** signed with a
   payment key, **Then** the transaction contains the correct key
   witness.
2. **Given** a Dijkstra transaction requiring delegation certificates,
   **When** signed, **Then** the estimated delegation signing keys are
   correct.

---

### User Story 3 - Estimate Dijkstra Transaction Size and Fees (Priority: P2)

A downstream consumer needs to estimate the size and minimum fee of a
Dijkstra-era transaction before committing to it. The library provides
accurate size estimation accounting for Dijkstra-specific transaction
body structure.

**Why this priority**: Fee estimation is used during the balancing
loop — if it's wrong, balancing fails or produces invalid transactions.
Tested implicitly by P1 but worth validating independently.

**Independent Test**: Can be tested by constructing a known Dijkstra
transaction and comparing estimated size against actual serialized size.

**Acceptance Scenarios**:

1. **Given** a Dijkstra transaction with known structure, **When** size
   is estimated, **Then** the estimate is within acceptable bounds of
   the actual serialized size.

---

### User Story 4 - Drop Babbage as a RecentEra (Priority: P3)

Once Dijkstra is promoted, Babbage is three eras behind the current era.
The library should support Conway and Dijkstra as the two recent eras,
dropping Babbage support from the write path.

**Why this priority**: Cleaning up old era support simplifies the
codebase but is not required for Dijkstra to work. Can be done as a
follow-up.

**Independent Test**: Can be tested by verifying that all existing
Conway tests still pass after Babbage removal, and that Babbage-era
operations are correctly rejected.

**Acceptance Scenarios**:

1. **Given** the library with Dijkstra promoted, **When** a consumer
   attempts to balance a Babbage-era transaction, **Then** the operation
   fails with a clear era-not-supported error rather than a runtime
   crash.

### Edge Cases

- What happens when a Dijkstra transaction contains the new `Guard`
  credential role — does size estimation account for it?
- How does the library handle transactions with Plutus V4 scripts
  (new in Dijkstra) — are language views computed correctly for the
  script integrity hash?
- What happens during the hard-fork transition window when both Conway
  and Dijkstra blocks may arrive — does the library handle both eras
  concurrently?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Library MUST support balancing Dijkstra-era transactions
  with the same capabilities as Conway (coin selection, fee estimation,
  change construction, redeemer assignment).
- **FR-002**: Library MUST support signing Dijkstra-era transactions
  including key witnesses and delegation certificate witness estimation.
- **FR-003**: Library MUST correctly estimate transaction size for
  Dijkstra-era transactions, accounting for the two additional TxBody
  fields and the Guard credential role.
- **FR-004**: Library MUST compute script integrity hashes correctly for
  Dijkstra transactions, including Plutus V4 language views.
- **FR-005**: Library MUST maintain backward compatibility — existing
  Conway and Babbage transaction balancing MUST continue to work
  unchanged.
- **FR-006**: Library MUST expose Dijkstra as a member of the RecentEra
  type so downstream consumers can write era-polymorphic code.
- **FR-007**: All existing tests MUST pass for Conway and Babbage eras
  after the change.
- **FR-008**: New tests MUST cover Dijkstra-era transaction balancing,
  signing, and size estimation.

### Key Entities

- **RecentEra**: A GADT with constructors for each supported era. Gains
  a `RecentEraDijkstra` constructor.
- **IsRecentEra**: A type class providing era-specific constraints and
  the `recentEra` singleton. Gains a Dijkstra instance.
- **RecentEraConstraints**: A constraint synonym bundling ledger type
  class requirements. Must be updated for Dijkstra's changed type
  equalities (Tx newtype, NativeScript vs TimelockScript).
- **MaybeInRecentEra**: An existential wrapper for era-indexed values.
  Gains `InRecentEraDijkstra` constructor.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing 268+ unit tests pass unchanged for Conway
  and Babbage eras.
- **SC-002**: New Dijkstra-era tests achieve equivalent coverage to
  existing Conway tests (balancing, signing, size estimation, surplus
  distribution).
- **SC-003**: `cabal build` and `cabal test` pass with no warnings
  (except pre-existing ones).
- **SC-004**: cardano-wallet can be updated to pin the new version and
  replace its Dijkstra `error` stubs with working implementations.

## Assumptions

- The Dijkstra ledger types are structurally similar enough to Conway
  that the existing balancing algorithm applies without fundamental
  changes.
- The `Core.Tx DijkstraEra` newtype wrapper (`MkDijkstraTx`) can be
  handled by relaxing or replacing the `Core.Tx era ~ AlonzoTx era`
  constraint in `RecentEraConstraints`.
- Plutus V4 language views follow the same pattern as V1-V3 for script
  integrity hash computation.
- The `Guard` credential role does not affect transaction balancing
  (it's a TxBody field but not relevant to coin selection or fee
  estimation beyond its contribution to transaction size).
