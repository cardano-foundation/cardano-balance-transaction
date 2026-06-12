---
name: cardano-balance-transaction-guide
description: >-
  Guide for working in the cardano-balance-transaction repository (the
  cardano-balance-tx Haskell library). Load when a task involves balancing
  Cardano transactions, the balanceTx entry point, coin selection, fee/size
  estimation, change construction, surplus distribution, token bundle size
  validation, Plutus redeemer reindexing, or the RecentEra (Conway/Dijkstra)
  abstraction. Triggers include: cardano-balance-tx, cardano-balance-transaction,
  balanceTx, PartialTx, ErrBalanceTx, UTxOIndex, ChangeAddressGen, RecentEra,
  IsRecentEra, RecentEraConway, RecentEraDijkstra, distributeSurplus,
  TokenBundleSizeAssessor, estimateTxSize, estimateTxCost, assignScriptRedeemers,
  TimeTranslation, UTxOAssumptions, cardano-coin-selection, cardano-ledger,
  lib/Cardano/Balance/Tx, "just build", "just unit", "nix run .#unit-tests",
  GHC 9.12.2, or questions about which Cardano eras this library supports.
---

# cardano-balance-transaction guide

Standalone Haskell library (no executable) that turns a partial Cardano
transaction into a fully balanced, ready-to-sign one. The single public
entry point is `balanceTx`. Targets the two most recent eras: **Conway
and Dijkstra**.

## Repository map

| Path | Purpose |
|------|---------|
| `lib/Cardano/Balance/Tx/Balance.hs` | `balanceTx` entry point, `ErrBalanceTx*` errors, `PartialTx`, `ChangeAddressGen`, `UTxOIndex`, `updateTx` |
| `lib/Cardano/Balance/Tx/Balance/CoinSelection.hs` | Adapter to `cardano-coin-selection` (selection params/constraints/strategy) |
| `lib/Cardano/Balance/Tx/Balance/Surplus.hs` | `distributeSurplus`, `TxFeeAndChange`, surplus delta math |
| `lib/Cardano/Balance/Tx/Balance/TokenBundleSize.hs` | `TokenBundleSizeAssessor`, `mkTokenBundleSizeAssessor` |
| `lib/Cardano/Balance/Tx/Eras.hs` | `RecentEra` GADT, `IsRecentEra`, `RecentEraConstraints`, `AnyRecentEra` (Conway, Dijkstra) |
| `lib/Cardano/Balance/Tx/Tx.hs` | `Tx`/`PParams` types, `serializeTx`/`deserializeTx`, `TxOut`, min-ada |
| `lib/Cardano/Balance/Tx/TxWithUTxO.hs` | `TxWithUTxO` (+ `construct`, `constructFiltered`) |
| `lib/Cardano/Balance/Tx/Sign.hs` | `estimateSignedTxSize`, `estimateSignedTxMinFee`, witness counting |
| `lib/Cardano/Balance/Tx/SizeEstimation.hs` | `estimateTxSize`, `estimateTxCost`, `TxSkeleton` |
| `lib/Cardano/Balance/Tx/Redeemers.hs` | `assignScriptRedeemers`, `ErrAssignRedeemers` |
| `lib/Cardano/Balance/Tx/TimeTranslation.hs` | `TimeTranslation`, `timeTranslationFromEpochInfo` |
| `lib/Cardano/Balance/Tx/UTxOAssumptions.hs` | `UTxOAssumptions`, `assumedInputScriptTemplate` |
| `lib/Cardano/Balance/Tx/Primitive.hs` + `Primitive/Convert.hs` | Lightweight value types (qualified `W`) and ledger conversions |
| `lib/Cardano/Balance/Tx/{Gen,Primitive/Gen,TxWithUTxO/Gen}.hs` | QuickCheck generators |
| `test/spec/` | Hspec/QuickCheck specs; `test/data/` | golden fixtures (`babbage/`, `conway/`, `dijkstra/`, `signedTxs/`) |
| `cabal.project` / `cardano-balance-tx.cabal` | Dependency pins (CHaP + GHC 9.12.2) and package metadata |
| `flake.nix` / `nix/project.nix` / `justfile` | Build tooling |
| `docs/` / `mkdocs.yml` | MkDocs site |

## Build, test, run

Everything routes through the flake (same as `.github/workflows/ci.yml`):

```bash
nix develop                  # dev shell (GHC 9.12.2, cabal, CHaP access)
just build                   # nix build .#lib .#unit-tests
just unit                    # nix run .#unit-tests
just unit "Surplus"          # run a matching subset
just CI                      # build + tests + fourmolu + hlint + nixfmt
just format                  # fourmolu, cabal-fmt, nixfmt
just docs-build              # mkdocs build --strict
```

Inside `nix develop`, cabal works too: `cabal build lib:cardano-balance-tx -O0`,
`cabal test unit -O0`.

## Navigating the code

- Start at `balanceTx` in `Balance.hs` (defined around line 529). Its
  internal worker is `balanceTxInner`. The module re-exports the selection,
  surplus, and error types callers need.
- The balancing loop is iterative: add inputs → re-estimate fee/size →
  recompute change → repeat until the fee stabilises, then assign
  redeemers and validate.
- Era handling is centralised in `Eras.hs`: pattern-match `RecentEra era`
  (`RecentEraConway` / `RecentEraDijkstra`); add era constraints via
  `IsRecentEra` / `RecentEraConstraints`. To support a new era you touch
  `Eras.hs` first.
- Ledger ↔ primitive conversions live in `Primitive/Convert.hs`
  (`toConwayTxOut`, `toDijkstraTxOut`, `toLedgerCoin`, …).

## Using cardano-balance-tx

Consume it as a library (`source-repository-package` in `cabal.project`).
The public surface is one function:

```haskell
balanceTx
    :: forall era m changeState
     . (MonadRandom m, IsRecentEra era)
    => PParams era
    -> TimeTranslation
    -> UTxOAssumptions
    -> UTxOIndex era
    -> ChangeAddressGen changeState
    -> changeState
    -> PartialTx era
    -> ExceptT (ErrBalanceTx era) m (Tx era, changeState)
```

- Build the `UTxOIndex era` with `constructUTxOIndex`.
- Failures come back as `ErrBalanceTx era` (e.g.
  `ErrBalanceTxAssetsInsufficientError`,
  `ErrBalanceTxUnableToCreateChangeError`,
  `ErrBalanceTxInsufficientCollateralError`).
- The era type parameter must satisfy `IsRecentEra` — instantiate it at
  `Conway` or `Dijkstra`.

## Answering questions

- **"What does this library do / how does balancing work?"** →
  `README.md` ("What is this", "Architecture") and
  `docs/architecture.md` (pipeline + iteration diagrams).
- **"Which eras are supported?"** → Conway and Dijkstra (the two most
  recent eras). Source of truth: `RecentEra` in `lib/Cardano/Balance/Tx/Eras.hs`.
  Babbage is a *non-recent* era kept only for serialization round-trip
  fixtures.
- **"How do I call it / what's the API?"** → README "Usage" and
  `docs/modules.md`; the signature is `balanceTx` in `Balance.hs`.
- **"How do I build/test it?"** → `README.md` "Development",
  `docs/getting-started.md`, the `justfile`, and `.github/workflows/ci.yml`.
- **"Where did it come from?"** → `NOTICE` and the README "Origin"
  section (extracted from `cardano-wallet` `lib/balance-tx/`).
- When a user claims a behaviour the docs don't cover, verify against the
  source under `lib/Cardano/Balance/Tx/` before answering.
