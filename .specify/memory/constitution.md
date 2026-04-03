# cardano-balance-tx Constitution

## Core Principles

### I. Ledger-Native Types

The library works directly with `cardano-ledger` types. No wallet-specific
type abstractions. This keeps the library independent and reusable by any
Cardano application that needs transaction balancing.

### II. Era-Indexed Correctness

All transaction operations are indexed by era via the `RecentEra` GADT and
`IsRecentEra` type class. This ensures era-specific logic is handled at the
type level — a transaction in era X cannot accidentally use era Y's rules.
The set of "recent eras" is the two most recent eras to support the
hard-fork transition window.

### III. Extracted Library Discipline

This library was extracted from cardano-wallet. It must remain a standalone,
independently testable package with no wallet-specific dependencies. Changes
must not introduce coupling back to the wallet.

### IV. Constraint-Driven Generics

Era-polymorphic code uses `RecentEraConstraints` to bundle the necessary
ledger type class constraints. When adding a new era, constraints that no
longer hold must be relaxed or replaced — never worked around with unsafe
coercions.

### V. Test Coverage

268+ unit tests via QuickCheck property testing. All era-specific behavior
must be tested for every supported era. New eras require corresponding test
coverage before the feature is considered complete.

## Quality Gates

- `cabal build` and `cabal test` must pass locally before pushing
- Fourmolu formatting (70-char line limit, leading commas/arrows)
- No `-Wno-*` suppressions without documented justification
- All public modules must have Haddock documentation

## Supported Downstream Consumers

- **cardano-wallet** — primary consumer, pins this library via
  `source-repository-package` in `cabal.project`
- **cardano-ledger-read** — sibling library providing the read layer

When making breaking changes (new `RecentEra` constructor, changed
constraints), coordinate with downstream pin updates.

**Version**: 1.0 | **Ratified**: 2026-04-03
