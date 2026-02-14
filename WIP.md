# Work in Progress

## Status

Repository skeleton created. Source extraction not yet started.

## Prerequisites

- [x] Create repository
- [x] Cachix secret configured
- [x] CI workflows
- [x] Nix flake + devShell
- [ ] Copy source from cardano-wallet `lib/balance-tx/`
- [ ] Replace `cardano-wallet-primitive` types with `cardano-ledger` types
- [ ] Replace `Ledger.Convert` with direct ledger type usage
- [ ] Inline small utilities (collateral, TxSize, generators)
- [ ] Adapt CoinSelection adapter (ledger ↔ coin-selection)
- [ ] Fix test suite (remove address-derivation-discovery, wallet-secrets)
- [ ] All 268 tests passing
- [ ] Update cardano-wallet to use standalone library

## Blocked on

- PR [#5193](https://github.com/cardano-foundation/cardano-wallet/pull/5193)
  (coin-selection extraction) — must merge first

## Notes

- Namespace: `Cardano.Balance.Tx.*`
- See [PLAN.md](PLAN.md) for full extraction plan
