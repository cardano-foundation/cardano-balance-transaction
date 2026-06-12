# Module Reference

## Balancing

### `Cardano.Balance.Tx.Balance`

Main entry point. Exports `balanceTx` and all error types
(`ErrBalanceTx`, `ErrBalanceTxAssetsInsufficientError`, etc.).

### `Cardano.Balance.Tx.Balance.CoinSelection`

Adapter bridging ledger types to the `cardano-coin-selection` library.
Converts between ledger UTxO/coin representations and the types expected
by the coin selection algorithm.

### `Cardano.Balance.Tx.Balance.Surplus`

Surplus distribution logic. Given excess ada after fee estimation,
decides how to split it between fee padding and change outputs.
Exports `distributeSurplus`, `sizeOfCoin`, `costOfIncreasingCoin`.

### `Cardano.Balance.Tx.Balance.TokenBundleSize`

Assesses whether a token bundle fits within the maximum serialized
length allowed by the ledger. Exports `TokenBundleSizeAssessor`,
`mkTokenBundleSizeAssessor`.

## Types

### `Cardano.Balance.Tx.Eras`

Era definitions. The `RecentEra` GADT covers the two most recent eras
— Conway and Dijkstra — with associated type families and constraints
(`IsRecentEra`, `RecentEraConstraints`, `AnyRecentEra`).

### `Cardano.Balance.Tx.Primitive`

Lightweight primitive types for transaction balancing, inlined from
`cardano-wallet-primitive`. Intended to be imported qualified as `W`.

### `Cardano.Balance.Tx.Primitive.Convert`

Conversions between primitive types and ledger types. Replaces
`Cardano.Wallet.Primitive.Ledger.Convert`. Exports `toLedgerCoin`,
`fromLedgerCoin`, `toLedgerTokenBundle`, `fromLedgerTokenBundle`.

### `Cardano.Balance.Tx.Tx`

Transaction and protocol-parameter types: `KeyWitnessCounts`,
`PParamsInAnyRecentEra`, transaction serialization (`serializeTx`,
`deserializeTx`), `TxOut` handling, and minimum-ada computation
(`computeMinimumCoinForTxOut`). The underlying `TxOut` representation is
the shared `BabbageTxOut`, reused by both recent eras (Conway and
Dijkstra).

### `Cardano.Balance.Tx.TxWithUTxO`

The `TxWithUTxO` data type — a transaction bundled with the UTxO
entries it references. Provides smart constructors `construct` and
`constructFiltered`.

## Utilities

### `Cardano.Balance.Tx.Redeemers`

Plutus redeemer index assignment. After coin selection changes the
input set, redeemer indices must be updated to match the new ordering.

### `Cardano.Balance.Tx.Sign`

Signing-related size and fee estimation required during balancing.
Exports `estimateSignedTxSize`, `estimateSignedTxMinFee`,
`KeyWitnessCounts`, `TimelockKeyWitnessCounts`, and the witness-count
estimators (`estimateKeyWitnessCounts`,
`estimateMaxWitnessRequiredPerInput`). (Actual transaction signing is
not yet implemented here — see the module's `TODO`.)

### `Cardano.Balance.Tx.SizeEstimation`

Transaction size and cost estimation. Predicts the serialized byte
size of a transaction before final encoding, used to estimate fees.
Exports `estimateTxSize`, `estimateTxCost`.

### `Cardano.Balance.Tx.TimeTranslation`

Slot/time translation from epoch info. Provides `TimeTranslation`
and `timeTranslationFromEpochInfo`.

### `Cardano.Balance.Tx.UTxOAssumptions`

UTxO script assumptions for size estimation. Defines what script
types are expected for UTxO entries when estimating witness sizes.

## Testing

### `Cardano.Balance.Tx.Gen`

QuickCheck generators for protocol parameters and datum hashes.

### `Cardano.Balance.Tx.Primitive.Gen`

QuickCheck generators for primitive types (`Coin`, `TokenBundle`,
etc.) with distributions matching the original wallet test suite.

### `Cardano.Balance.Tx.TxWithUTxO.Gen`

Generators and shrinkers for `TxWithUTxO`, including variants with
minimal and surplus UTxO.
