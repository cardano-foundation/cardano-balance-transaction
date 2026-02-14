# Extraction Plan: `cardano-balance-tx`

Extract `lib/balance-tx/` from
[cardano-wallet](https://github.com/cardano-foundation/cardano-wallet)
into this standalone repository.

## Context

PR [#5193](https://github.com/cardano-foundation/cardano-wallet/pull/5193)
established the pattern: extract `lib/coin-selection/` into standalone
[cardano-coin-selection](https://github.com/cardano-foundation/cardano-coin-selection)
with zero wallet deps, then adapt balance-tx with a `toCS*/fromCS*`
conversion layer. This is the next step — extracting balance-tx itself.

### Key difference from coin-selection extraction

`cardano-coin-selection` could go zero-dependency because it's a pure
algorithm. `cardano-balance-tx` **cannot** — it constructs real Cardano
transactions, so it must keep `cardano-ledger-*`, `cardano-api`, and
`ouroboros-consensus-*` dependencies. The goal is to drop
**wallet-specific** deps only.

## Namespace

**`Cardano.Balance.Tx.*`** — clean standalone namespace, matching
`Cardano.CoinSelection.*` from coin-selection.

## Phase 1: Eliminate `cardano-wallet-primitive` dependency

This is the core challenge. balance-tx uses wallet-primitive for two things:

### A. Primitive types

Strategy: **use cardano-ledger types directly** instead of maintaining a
parallel type system.

balance-tx already depends on all `cardano-ledger-*` packages. The
wallet-primitive types are thin wrappers (`newtype Coin = Coin Natural`,
`newtype Address = Address ByteString`) with a conversion layer
(`Ledger.Convert`) to/from ledger types. Cut out the middleman:

| Wallet type | Replace with |
|---|---|
| `W.Coin` | `Ledger.Coin` (from `cardano-ledger-core`) |
| `W.Address` | `Addr StandardCrypto` or `Ledger.Addr` |
| `W.TxIn` | `Ledger.TxIn StandardCrypto` |
| `W.TxOut` | `BabbageTxOut`/`ConwayTxOut` (era-specific) |
| `W.UTxO` | `Ledger.UTxO era` |
| `W.TokenBundle` | Reuse from `cardano-coin-selection` |
| `W.TokenMap`, `W.AssetId`, etc. | Reuse from `cardano-coin-selection` |

### B. `Ledger.Convert` module

Becomes unnecessary once balance-tx uses ledger types natively.

### C. Small utilities — inline

- `Cardano.Wallet.Primitive.Collateral.asCollateral` — trivial ada-only
  check
- `Cardano.Wallet.Primitive.Types.Tx.Constraints` — `TxSize`,
  `txOutMaxCoin`, `txOutMaxTokenQuantity`

## Phase 2: Eliminate `cardano-wallet-test-utils` dependency

Same pattern as coin-selection extraction:

- QuickCheck generators for Coin, TokenBundle, UTxO, etc. — inline into
  local `Gen` modules (or reuse coin-selection's generators where types
  match)
- Test law helpers — inline the few used

## Phase 3: Eliminate test-only wallet dependencies

The test suite (`BalanceSpec.hs`) has the heaviest wallet coupling:

| Dependency | Resolution |
|---|---|
| `address-derivation-discovery` | Direct ledger address construction |
| `cardano-wallet-secrets` | Direct `cardano-api` signing |
| `Cardano.Wallet.Address.*` | Simple address generator callback |
| `Cardano.Wallet.Primitive.NetworkId` | `cardano-api`'s `NetworkId` |
| `Cardano.Wallet.Unsafe.unsafeFromHex` | Inline (trivial) |

## Phase 4: Public API

Expose a clean API using ledger types. The **wallet** becomes the
consumer with its own thin adapter layer (the inverse of today).

```haskell
-- Current (wallet-coupled)
balanceTx :: ... -> W.UTxO -> PartialTx era -> ... -> Either ErrBalanceTx (Tx era)

-- Standalone (ledger-native)
balanceTx :: ... -> UTxO era -> PartialTx era -> ... -> Either ErrBalanceTx (Tx era)
```

## Phase 5: CoinSelection adapter layer

The `CoinSelection.hs` adapter bridges **ledger types ↔ coin-selection
types** (instead of wallet types ↔ coin-selection types). The
`toCS*/fromCS*` pattern stays, but the "from" side changes. Most
conversions simplify.

## Module structure

```
lib/
  Cardano/Balance/Tx.hs                    -- public API
  Cardano/Balance/Tx/
    Balance.hs                             -- main balancing algorithm
    Balance/
      CoinSelection.hs                     -- coin-selection adapter
      Surplus.hs                           -- surplus distribution
      TokenBundleSize.hs                   -- bundle size estimation
    Eras.hs                                -- era definitions
    Redeemers.hs                           -- redeemer assignment
    Sign.hs                                -- transaction signing
    SizeEstimation.hs                      -- size estimation
    TimeTranslation.hs                     -- slot/time translation
    Types.hs                               -- TxSize, constraints
test/
  ...                                      -- mirrors lib structure
```

## Execution order

1. Copy source from `lib/balance-tx/` into this repo
2. Replace wallet-primitive types with ledger types throughout
3. Remove `Ledger.Convert` usage, work with ledger types directly
4. Inline small utilities (collateral check, TxSize, generators)
5. Adapt CoinSelection adapter from wallet↔CS to ledger↔CS
6. Fix test suite — replace address derivation/signing with ledger/api
7. Get tests green (268 examples target)
8. Update cardano-wallet to depend on standalone library

## Risk areas

- **`Ledger.Convert` is deeply woven** — ~10 files, mechanical but large
  diff
- **BalanceSpec.hs is heavy** — uses Byron/Shelley address derivation
  extensively
- **Golden tests** — may need regeneration with ledger types
- **Downstream wallet consumers** — api, network-layer, benchmarks all
  use `cardano-balance-tx:internal`
