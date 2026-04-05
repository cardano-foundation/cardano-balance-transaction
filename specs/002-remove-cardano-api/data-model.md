# Data Model: Remove cardano-api dependency

This feature is a dependency migration — no new data entities are
introduced. The following documents the type-level changes to existing
entities.

## Entity: RecentEra (Eras.hs)

### Before

```
type family CardanoApiEra era = cardanoApiEra | cardanoApiEra -> era
type instance CardanoApiEra Conway = CardanoApi.ConwayEra
type instance CardanoApiEra Dijkstra = CardanoApi.DijkstraEra

class (CardanoApi.IsShelleyBasedEra (CardanoApiEra era),
       CardanoApi.ShelleyLedgerEra (CardanoApiEra era) ~ era,
       RecentEraConstraints era)
  => IsRecentEra era
```

### After

```
class (RecentEraConstraints era) => IsRecentEra era
```

**Removed**: `CardanoApiEra` type family, `IsShelleyBasedEra` constraint,
`ShelleyLedgerEra` constraint, `cardanoEraFromRecentEra`,
`shelleyBasedEraFromRecentEra`, `toAnyCardanoEra`, `fromAnyCardanoEra`.

**Retained**: `RecentEra` GADT, `IsRecentEra` class,
`RecentEraConstraints`, `AnyRecentEra`, `MaybeInRecentEra` (with
non-recent constructors converted to a simple rejection value or
removed).

## Entity: Tx serialization (Tx.hs)

### Before

```
serializeTx tx = CardanoApi.serialiseToCBOR (toCardanoApiTx tx)
deserializeTx = fromCardanoApiTx . CardanoApi.deserialiseFromCBOR ...
```

### After

```
serializeTx tx = BSL.toStrict (serialize version tx)
deserializeTx bs = decodeFull version bs
```

**Removed**: `toCardanoApiTx`, `fromCardanoApiTx`, `toRecentEraGADT`
(the `AnyCardanoEra` branch).

## Entity: KeyWitnessCounts (Sign.hs)

### Before

Computed by `CardanoApi.getTxBodyContent` → pattern match on
`TxInsCollateral`, `TxExtraKeyWitnesses`, `TxWithdrawals`,
`TxCertificates`, etc.

### After

Computed by ledger lenses on the transaction body directly:
`inputsTxBodyL`, `collateralInputsTxBodyL`, `reqSignerHashesTxBodyL`,
`withdrawalsTxBodyL`, `certsTxBodyL`.

**Removed**: `txUpdateProposal` handling (not applicable in
Conway/Dijkstra).

## Import Replacement Map

| Current import | Replacement | Package |
|---|---|---|
| `Cardano.Api.Ledger (Coin)` | `Cardano.Ledger.Coin` | cardano-ledger-core |
| `Cardano.Api.Ledger (PParams, ..)` | `Cardano.Ledger.Core` | cardano-ledger-core |
| `Cardano.Api.Ledger (CostModels)` | `Cardano.Ledger.Alonzo.Scripts` | cardano-ledger-alonzo |
| `Cardano.Api (serialiseToCBOR)` | `Cardano.Ledger.Binary (serialize)` | cardano-ledger-binary |
| `Cardano.Api (deserialiseFromCBOR)` | `Cardano.Ledger.Binary (decodeFull)` | cardano-ledger-binary |
| `Cardano.Api (Tx, ShelleyTx)` | Direct `Ledger.Tx` | cardano-ledger-api |
| `Cardano.Api (ConwayEra, DijkstraEra)` | `Cardano.Ledger.Api` | cardano-ledger-api |
| `Cardano.Api (IsShelleyBasedEra)` | Removed (redundant) | — |
| `Cardano.Api (ShelleyLedgerEra)` | Removed (identity) | — |
| `Cardano.Api (AnyCardanoEra)` | Local enum or removed | — |
| `Cardano.Api (getTxBodyContent)` | Ledger lenses | cardano-ledger-alonzo |
| `Cardano.Api (txIns, txInsCollateral, ..)` | Ledger lenses | cardano-ledger-babbage |
| `Cardano.Api (SlotNo)` | `Cardano.Slotting.Slot` | cardano-slotting |
| `Cardano.Api.Experimental.Certificate` | `Cardano.Ledger.Conway.TxCert` | cardano-ledger-conway |
