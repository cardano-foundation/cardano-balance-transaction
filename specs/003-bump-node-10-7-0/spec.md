# Feature Specification: Bump cardano-node to 10.7.0

**Feature Branch**: `003-bump-node-10-7-0`
**Created**: 2026-04-15
**Status**: Draft
**Input**: User description: "Finish the remaining build fixes after the
cardano-node 10.7.0 dependency bump"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Library compiles on ledger 1.19 (Priority: P1)

A maintainer upgrades to the `cardano-node 10.7.0` dependency set and
the library builds successfully against ledger `1.19` without manual
patching.

**Why this priority**: A non-compiling library blocks all downstream
use and review. This is the minimum acceptable outcome for the bump.

**Independent Test**: `nix develop --quiet -c cabal build lib:cardano-balance-tx -O0`

**Acceptance Scenarios**:

1. **Given** the `10.7.0` dependency set, **When** the library is
   built, **Then** recent-era native script conversion and witness
   estimation compile for both Conway and Dijkstra.
2. **Given** the updated ledger APIs, **When** the era constraints are
   checked, **Then** the recent-era abstraction does not depend on the
   removed `Timelock` equality assumptions.

---

### User Story 2 - Tests compile with current ledger packages (Priority: P2)

A maintainer builds the test suite and no longer depends on removed
`testlib` sublibraries from `cardano-ledger-babbage` and
`cardano-ledger-conway`.

**Why this priority**: The migration is incomplete if the library builds
but test support code still targets removed packages.

**Independent Test**: `nix develop --quiet -c cabal build test:unit -O0`

**Acceptance Scenarios**:

1. **Given** the current ledger package set, **When** test modules are
   compiled, **Then** all `Arbitrary` imports resolve from supported
   modules or local generators.
2. **Given** the project constitution, **When** test generators are
   updated, **Then** they remain self-contained within the project or
   supported ledger packages.

---

### User Story 3 - Full project builds cleanly after the bump (Priority: P3)

A reviewer checks the bump branch and sees a clean full build against
the updated flake and cabal bounds.

**Why this priority**: This is the integration proof that the migration
is complete and ready for PR review.

**Independent Test**: `nix develop --quiet -c cabal build all -O0`

**Acceptance Scenarios**:

1. **Given** the full package graph, **When** all components are built,
   **Then** the library and test suite compile without ledger API
   errors.
2. **Given** the migration branch, **When** the remaining changes are
   reviewed, **Then** they are grouped into focused, reviewable
   increments aligned with the existing bump commits.

### Edge Cases

- What happens when Conway and Dijkstra use different native script
  representations for the same recent-era helper?
- What happens when generator instances moved from removed `testlib`
  sublibraries are no longer exposed anywhere upstream?
- What happens when ledger `1.19` API changes surface only after an
  earlier compatibility fix removes the first compilation blocker?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The library MUST compile against the dependency set pinned
  for `cardano-node 10.7.0`.
- **FR-002**: Recent-era native script conversion MUST work for both
  Conway and Dijkstra, despite Dijkstra using
  `DijkstraNativeScript` instead of `Timelock`.
- **FR-003**: The library MUST remove stale imports and constraints that
  assume `Core.NativeScript era ~ Timelock era` for every recent era.
- **FR-004**: Test code MUST stop depending on removed
  `cardano-ledger-babbage:testlib` and
  `cardano-ledger-conway:testlib` sublibraries.
- **FR-005**: All remaining ledger-core `1.19` API changes encountered
  during the bump MUST be adapted in project code.
- **FR-006**: The migration MUST preserve the two-era discipline defined
  in the project constitution.

### Key Entities

- **RecentEra**: The project’s closed set of supported ledger eras,
  currently Conway and Dijkstra.
- **Native Script Conversion**: The conversion helpers between
  wallet-level scripts and ledger-native script values.
- **Ledger Arbitrary Support**: The source of QuickCheck instances used
  by the unit test suite for ledger types.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `nix develop --quiet -c cabal build lib:cardano-balance-tx -O0`
  succeeds.
- **SC-002**: `nix develop --quiet -c cabal build test:unit -O0`
  succeeds.
- **SC-003**: `nix develop --quiet -c cabal build all -O0` succeeds.
- **SC-004**: No source file imports a removed ledger `testlib`
  sublibrary.

## Assumptions

- The branch’s existing dependency bumps and CHaP alignment are correct,
  and the remaining work is strictly source compatibility.
- The project may replace removed upstream test generators with local
  orphan instances where necessary, consistent with the constitution.
- The current PR does not need a broader redesign beyond adapting to the
  ledger `1.19` API surface.
