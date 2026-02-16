# cardano-balance-tx

[![CI](https://github.com/cardano-foundation/cardano-balance-transaction/actions/workflows/ci.yml/badge.svg)](https://github.com/cardano-foundation/cardano-balance-transaction/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

Standalone Cardano transaction balancing library, extracted from
[cardano-wallet](https://github.com/cardano-foundation/cardano-wallet).

## Overview

`cardano-balance-tx` takes a partial Cardano transaction (with
user-specified outputs but incomplete inputs and fees) and produces a
fully balanced, ready-to-sign transaction. It handles:

- **Coin selection** — choosing UTxO inputs to cover outputs, fees, and
  collateral (via
  [cardano-coin-selection](https://github.com/cardano-foundation/cardano-coin-selection))
- **Fee estimation** — computing minimum fees from transaction size and
  protocol parameters
- **Change output construction** — distributing surplus ada and native
  tokens back to the wallet
- **Surplus distribution** — splitting fee surplus between fee padding
  and change outputs
- **Token bundle size validation** — ensuring outputs don't exceed
  ledger limits
- **Redeemer assignment** — updating Plutus script redeemer indices
  after input selection
- **Transaction size estimation** — predicting serialized size before
  final encoding
- **Multi-era support** — Babbage and Conway eras

The library works directly with `cardano-ledger` types, avoiding any
wallet-specific type abstractions.

## Modules

| Module | Description |
|--------|-------------|
| `Cardano.Balance.Tx.Balance` | Main `balanceTx` entry point and error types |
| `Cardano.Balance.Tx.Balance.CoinSelection` | Adapter bridging ledger types to coin-selection |
| `Cardano.Balance.Tx.Balance.Surplus` | Surplus distribution between fees and change |
| `Cardano.Balance.Tx.Balance.TokenBundleSize` | Token bundle size assessment |
| `Cardano.Balance.Tx.Eras` | Era definitions and constraints (`RecentEra`) |
| `Cardano.Balance.Tx.Gen` | QuickCheck generators for protocol parameters and datums |
| `Cardano.Balance.Tx.Primitive` | Lightweight primitive types (qualified import as `W`) |
| `Cardano.Balance.Tx.Primitive.Convert` | Conversions between primitive and ledger types |
| `Cardano.Balance.Tx.Primitive.Gen` | QuickCheck generators for primitive types |
| `Cardano.Balance.Tx.Redeemers` | Plutus redeemer index assignment |
| `Cardano.Balance.Tx.Sign` | Transaction signing utilities |
| `Cardano.Balance.Tx.SizeEstimation` | Transaction size and cost estimation |
| `Cardano.Balance.Tx.TimeTranslation` | Slot/time translation from epoch info |
| `Cardano.Balance.Tx.Tx` | Transaction types and key witness counting |
| `Cardano.Balance.Tx.TxWithUTxO` | Transaction paired with its resolved UTxO |
| `Cardano.Balance.Tx.TxWithUTxO.Gen` | Generators for `TxWithUTxO` |
| `Cardano.Balance.Tx.UTxOAssumptions` | UTxO script assumptions for size estimation |

## Quick start

```bash
# Enter the Nix devShell
nix develop

# Build
cabal build lib:cardano-balance-tx -O0

# Run tests (268 examples)
cabal test unit -O0
```

## How it works

The balancing algorithm follows a pipeline:

1. **Select inputs** — run coin selection to pick UTxO entries that
   cover the requested outputs plus estimated fees
2. **Estimate fees** — compute the minimum fee from the partially
   assembled transaction and protocol parameters
3. **Construct change** — distribute surplus ada and native tokens into
   change outputs, respecting token bundle size limits
4. **Assign redeemers** — reindex Plutus redeemer pointers to reflect
   the final input ordering
5. **Validate** — check the balanced transaction against ledger rules

The algorithm iterates when fee estimates change after adding inputs
(more inputs = larger transaction = higher fee), converging when the fee
stabilizes.

## Supported eras

- Babbage
- Conway

## Key dependencies

- `cardano-ledger-*` — core ledger types and validation
- `cardano-api` — high-level Cardano API
- `cardano-coin-selection` — coin selection algorithm
- `ouroboros-consensus` — consensus protocol types

## Origin

Extracted from `lib/balance-tx/` in
[cardano-wallet](https://github.com/cardano-foundation/cardano-wallet)
(PR [#5193](https://github.com/cardano-foundation/cardano-wallet/pull/5193)
established the extraction pattern). See [NOTICE](NOTICE) for original
authors and credits.

## License

Apache-2.0 — see [LICENSE](LICENSE).

Copyright 2018-2022 IOHK, 2023-2026 Cardano Foundation.
