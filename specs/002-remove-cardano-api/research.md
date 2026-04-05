# Research: Remove cardano-api dependency

## R1: Cardano.Api.Ledger re-exports — can they all be replaced?

**Decision**: Yes. All types imported via `Cardano.Api.Ledger` are
re-exports from `cardano-ledger-*` packages already in `build-depends`.

**Rationale**: Verified by reading
`cardano-api/src/Cardano/Api/Ledger/Internal/Reexport.hs` which
explicitly re-exports from:
- `Cardano.Ledger.Coin` → `Coin`
- `Cardano.Ledger.TxIn` → `TxIn`, `TxId`
- `Cardano.Ledger.Core` → `PParams`, `PParamsUpdate`
- `Cardano.Ledger.Binary` → `toCBOR`, `fromCBOR`, `serialize'`
- `Cardano.Ledger.Api` → `ConwayEra`, `DijkstraEra`

**Alternatives considered**: None — direct imports are strictly better.

## R2: TxBodyContent replacement strategy

**Decision**: Use ledger lenses from `Cardano.Ledger.Alonzo.Core` and
`Cardano.Ledger.Babbage.TxBody`.

**Rationale**: `Sign.hs` uses `CardanoApi.getTxBodyContent` to get a
denormalized record, then pattern-matches on ~8 constructors. The ledger
has no equivalent record, but provides lenses:

| CardanoApi accessor | Ledger lens |
|---|---|
| `txIns txbodycontent` | `bodyTxL . inputsTxBodyL` |
| `txInsCollateral txbodycontent` | `bodyTxL . collateralInputsTxBodyL` |
| `txExtraKeyWits txbodycontent` | `bodyTxL . reqSignerHashesTxBodyL` |
| `txWithdrawals txbodycontent` | `bodyTxL . withdrawalsTxBodyL` |
| `txCertificates txbodycontent` | `bodyTxL . certsTxBodyL` |
| `txUpdateProposal txbodycontent` | N/A (deprecated, no Conway/Dijkstra equivalent) |

The `txUpdateProposal` handling can be removed — update proposals don't
exist in Conway/Dijkstra eras (replaced by governance actions). The
current code returns 0 for non-matching patterns anyway.

**Alternatives considered**: Introducing a local `TxBodyContent`-like
record — rejected as unnecessary indirection.

## R3: CBOR serialization equivalence

**Decision**: Use `Cardano.Ledger.Binary.serialize` directly on
`Ledger.Tx`.

**Rationale**: `CardanoApi.serialiseToCBOR` on `CardanoApi.Tx` calls
`toCBOR` on the inner `Ledger.Tx` wrapped in `ShelleyTx`. The
`ShelleyTx` wrapper adds no extra CBOR envelope — it delegates directly
to the ledger type's `ToCBOR` instance. Therefore
`Cardano.Ledger.Binary.serialize` on the same `Ledger.Tx` value
produces identical bytes.

For deserialization, `Cardano.Ledger.Binary.decodeFull` or
`decodeFullAnnotator` can replace `CardanoApi.deserialiseFromCBOR`.

**Verification**: A golden test comparing serialization output before
and after the switch will confirm byte-identity.

**Alternatives considered**: Using `Codec.CBOR.Write` directly —
rejected because `Cardano.Ledger.Binary` already wraps it with
era-aware encoding.

## R4: CardanoApiEra type family removal

**Decision**: Remove the `CardanoApiEra` type family and all
`cardano-api` era types from `Eras.hs`.

**Rationale**: The type family maps `Conway → CardanoApi.ConwayEra` and
`Dijkstra → CardanoApi.DijkstraEra`. Since the internal era types ARE
the ledger era types (`type Conway = ConwayEra` from
`Cardano.Ledger.Api`), the mapping is an identity with extra wrapping.

The `IsRecentEra` superclass constraints `IsShelleyBasedEra` and
`ShelleyLedgerEra ~ era` are redundant — the `RecentEraConstraints` type
alias already captures all needed ledger constraints (`Core.Era era`,
`Core.EraTx era`, `Babbage.BabbageEraTxBody era`, etc.).

The `AnyCardanoEra` conversions (`toAnyCardanoEra`, `fromAnyCardanoEra`,
`MaybeInRecentEra` non-recent constructors) exist for legacy wallet
compatibility. They can be replaced with a local `AnyRecentEra` that
only covers Conway and Dijkstra — the non-recent era constructors in
`MaybeInRecentEra` become simple string tags or are removed.

**Public API break**: `CardanoApiEra`, `toCardanoApiTx`,
`fromCardanoApiTx`, `toAnyCardanoEra`, `fromAnyCardanoEra` will be
removed. FR-008 covers downstream migration tickets.

**Alternatives considered**: Keeping a compatibility module — rejected
because it would re-introduce the dependency or duplicate types.

## R5: StandardCrypto import

**Decision**: Replace `Ouroboros.Consensus.Shelley.Eras (StandardCrypto)`
with `Cardano.Protocol.Crypto (StandardCrypto)` from
`cardano-protocol-tpraos` — a ledger-ecosystem package.

**Rationale**: `StandardCrypto` is defined in `Cardano.Protocol.Crypto`
(package `cardano-protocol-tpraos`), not in ouroboros-consensus.
The ouroboros module re-exports it. Using the original source is cleaner
and may help narrow the ouroboros dependency in a future effort.

Note: `ouroboros-consensus` remains needed for `PastHorizonException`
(used in `TimeTranslation.hs` for validity interval calculations).
That's a separate concern outside this ticket's scope.

## R6: Certificate handling in Sign.hs

**Decision**: Use `Cardano.Ledger.Conway.TxCert` and
`Cardano.Ledger.Dijkstra.TxCert` directly (already imported).

**Rationale**: `Sign.hs` uses `Cardano.Api.Experimental.Certificate` for
`estimateDelegSigningKeys`. The function pattern-matches on certificate
types to count delegation signing keys. The same certificate constructors
are available from the already-imported ledger TxCert modules. The
mapping is 1:1.

## R7: Test generator strategy

**Decision**: Rewrite generators against ledger types in-place.

**Rationale**: The `Cardano/Api/Gen.hs` test module (~500 lines) provides
QuickCheck generators for addresses, scripts, certificates, etc. using
`cardano-api` types. Since the library operates on ledger types, the
generators should produce ledger types directly. Some generators exist
in `cardano-ledger-test` but that package may pull in unwanted deps —
writing self-contained generators is safer and aligns with Constitution
Principle IV.

The module can be renamed from `Cardano.Api.Gen` to
`Cardano.Balance.Tx.Test.Gen` to reflect its new dependency-free nature.
