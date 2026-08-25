# Tasks — cardano-balance-transaction#45

Slice **S1** (OWNER).

- [ ] **T001** `cabal.project` index-states + freeze constraint
      table. Old 11.0.1 values gone. `allow-newer` untouched unless
      the solver requires a recorded exception.
- [ ] **T002** Resolve and pin `cardano-ledger-alonzo-test` and
      `cardano-ledger-shelley-ma-test` at the new index-state.
      Record landed versions.
- [ ] **T003** `.cabal` freeze-lower / next-minor-upper.
- [ ] **T004** `flake.nix` indexState; `flake.lock` CHaP
      `c0770200…`; hackage snapshot `1d6c4337…` if Cabal-7159.
- [ ] **T005** Mechanical Haskell adaptation. Report dijkstra
      0.3.0.0 surface. Hunt unpinned seed-like values.
- [ ] **T006** fourmolu + hlint on touched Haskell.
- [ ] **T007** `nix develop` `cabal build all --enable-tests -O0`
      and `cabal test unit --enable-tests -O0`. Confirm
      coin-selection SRP needs no bump (or report).
