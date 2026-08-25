# Modules model — cardano-balance-transaction#45

No new modules. Existing owners stay.

Likely mechanical touch: `lib/Cardano/Balance/Tx/Redeemers.hs`
(`DijkstraSpending`/`Minting`/`Rewarding`), `Eras.hs` (`RecentEra
Dijkstra`), and any file the compiler names for consensus 4 /
crypto-class 2.5 / alonzo 1.16.

#21 APIs `sizeSignKeyVRF`, `IsValid`, `TPraos.BHeader` are
**absent** here (control: `Cardano.Crypto.Hash` is present). Do not
edit files to "apply" those adaptations without a compiler error.
