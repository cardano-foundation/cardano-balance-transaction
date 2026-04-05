<!-- Sync Impact Report
Version change: 0.0.0 → 1.0.0
Added sections: All (initial constitution)
Modified principles: N/A (first version)
Templates requiring updates:
  - .specify/templates/plan-template.md ✅ reviewed (Constitution Check aligns)
  - .specify/templates/spec-template.md ✅ reviewed (no updates needed)
  - .specify/templates/tasks-template.md ✅ reviewed (phase structure compatible)
Follow-up TODOs: None
-->

# cardano-balance-tx Constitution

## Core Principles

### I. Minimal Dependency Surface

The library MUST minimize its transitive dependency footprint. Direct
dependencies on high-level aggregation packages (e.g. `cardano-api`) are
prohibited when lower-level alternatives exist in already-depended-upon
packages. Every dependency MUST justify its presence — if a type or
function is available from a `cardano-ledger-*` package already in the
dependency list, that source MUST be preferred.

### II. Ledger-Native Types

All public APIs MUST operate on `cardano-ledger-*` types directly.
Wrapper types from external packages (e.g. `CardanoApi.Tx`,
`AnyCardanoEra`) MUST NOT appear in the library's public interface.
Interop adapters, if needed, belong in separate compatibility modules
clearly marked as such and excluded from the core API surface.

### III. Era Discipline

The library supports exactly the two most recent Cardano eras (the
"recent eras"). Era-polymorphic code MUST use the `IsRecentEra`
typeclass. Era-specific behavior MUST be confined to pattern matches on
`RecentEra` values. Adding or dropping an era MUST be a mechanical
change touching only the `Eras` module and its instances.

### IV. Property-Based Testing

All core algorithms (coin selection, fee estimation, surplus
distribution, redeemer assignment, size estimation) MUST have QuickCheck
property tests. Test generators MUST be self-contained within the
project — depending on external generator libraries that pull in
unnecessary transitive dependencies is prohibited. Generators for ledger
types MUST use `cardano-ledger-*` types directly.

### V. Behavioral Preservation

Refactoring (including dependency removal) MUST NOT change observable
behavior. The existing test suite is the behavioral contract. Any
internal rewrite MUST pass all existing tests without modification to
test assertions. If a test needs updating, the behavioral change MUST be
documented and justified.

### VI. Incremental Migration

Large-scale changes (e.g. removing a foundational dependency) MUST be
broken into independently reviewable, independently testable increments.
Each increment MUST leave the project in a compilable, all-tests-passing
state. Partial migrations with temporary compatibility shims are
acceptable as intermediate states.

## Dependency Policy

- **Allowed**: `cardano-ledger-*`, `cardano-coin-selection`,
  `ouroboros-consensus` (for `StandardCrypto` only), `cardano-crypto-*`,
  `cardano-addresses`, base Haskell ecosystem packages.
- **Prohibited after migration**: `cardano-api` in any form
  (`Cardano.Api`, `Cardano.Api.Ledger`, `Cardano.Api.Experimental.*`).
- **Boundary**: The library MUST be buildable without any
  `cardano-wallet`, `cardano-node`, or `cardano-api` package in scope.

## Development Workflow

- **Haskell style**: Fourmolu formatting, explicit imports, qualified
  imports for external modules.
- **Build system**: Nix flake with `haskell.nix`, cabal as the build
  tool. `justfile` for common commands.
- **CI**: GitHub Actions. All PRs MUST pass CI before merge.
- **Commits**: Small, focused, one logical change per commit.
- **Branches**: Feature branches from `main`, squash-merge or rebase.

## Governance

This constitution is the authority for architectural decisions in
`cardano-balance-tx`. All PRs MUST be checked against these principles.
Amendments require a PR modifying this file with rationale in the commit
message. Version follows semver:
- MAJOR: Principle removal or redefinition.
- MINOR: New principle or material expansion.
- PATCH: Clarification or wording fix.

**Version**: 1.0.0 | **Ratified**: 2026-04-05 | **Last Amended**: 2026-04-05
