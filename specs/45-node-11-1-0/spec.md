# Spec — cardano-balance-transaction#45

Move this repository's hardcoded pin set from `cardano-node`
11.0.1 to 11.1.0. Freeze sha256
`6a636c62ed1793c8dc8157633e1a907300efebdf9e98c4708cc8cbf8f317bab4`,
570 lines. Resolver output. Do not improve from bounds.

Issue: https://github.com/cardano-foundation/cardano-balance-transaction/issues/45

## Functional requirements

- **FR-INDEX.** Both `cabal.project` index-state occurrences:
  hackage `2026-08-14T13:38:07Z`, CHaP `2026-08-17T11:39:16Z`.
- **FR-CONSTRAINTS.** Freeze-pinned members become:

  | package | pin |
  |---|---|
  | cardano-addresses | `==4.0.2` (unchanged) |
  | cardano-binary | `==1.9.1.0` |
  | cardano-crypto-class | `==2.5.1.0` |
  | cardano-crypto-wrapper | `==1.7.0.0` (unchanged) |
  | cardano-ledger-allegra | `==1.10.0.0` |
  | cardano-ledger-alonzo | `==1.16.0.0` |
  | cardano-ledger-api | `==1.14.0.0` |
  | cardano-ledger-babbage | `==1.14.0.0` |
  | cardano-ledger-binary | `==1.9.0.0` |
  | cardano-ledger-byron | `==1.3.0.0` (unchanged) |
  | cardano-ledger-conway | `==1.23.0.0` |
  | cardano-ledger-core | `==1.21.0.0` |
  | cardano-ledger-dijkstra | `==0.3.0.0` |
  | cardano-ledger-mary | `==1.11.0.0` |
  | cardano-ledger-shelley | `==1.19.0.0` |
  | cardano-protocol-tpraos | `==1.6.0.0` |
  | cardano-slotting | `==0.2.1.0` (unchanged) |
  | cardano-strict-containers | `==0.1.6.0` (unchanged) |
  | ouroboros-consensus | `==4.1.0.0` |

  Leave `base >= 4.18.2.0 && < 5` and `openapi3 >= 3.2.0`.
  Leave `allow-newer` and the coin-selection SRP unless the solve
  forces a report (do not bump the SRP in this ticket).

- **FR-TEST-PINS.** `cardano-ledger-alonzo-test` and
  `cardano-ledger-shelley-ma-test` are **not in the node freeze**
  (control: freeze has `cardano-ledger-core ==1.21.0.0`; those two
  names are absent). Resolve them at the new index-state and keep
  `==` pins. Record the landed versions in the receipt.
- **FR-BOUNDS.** Library `build-depends` freeze-pinned packages:
  lower = freeze version, upper = next minor (`cardano-deps` §6).
- **FR-CHAP.** `flake.lock` CHaP rev
  `c0770200fcaa899ecdef548183dfee78e17bfb57`.
- **FR-NIX-INDEX.** `flake.nix` `indexState` =
  `2026-08-14T13:38:07Z`.
- **FR-HACKAGE.** If haskell.nix plan-to-nix Cabal-7159s because
  hackage.nix is older than the index-state, bump **only**
  `haskellNix/hackage` to
  `1d6c4337df9348ef6a1c21f7dfd47e9b9a454ccf` (node 11.1.0). Do not
  bump `haskellNix` itself unless that fails (then Q).
- **FR-BUILD.** Repo own `nix develop --quiet --accept-flake-config
  --no-write-lock-file -c cabal build all --enable-tests -O0`.
- **FR-TEST.** Same shell, `cabal test unit --enable-tests -O0`.
  Failures recorded with cause.
- **FR-ADAPT.** Mechanical only. Same five #21 adaptations if this
  tree uses those APIs. Hunt unpinned values the adaptation
  changes; if old==new, one assertion proven able to fail; if
  different, HS-BEHAVIOUR.
- **FR-DIJKSTRA.** Any `cardano-ledger-dijkstra` 0.3.0.0 surface
  change is reported explicitly (constructors
  `DijkstraSpending`/`DijkstraMinting`/`DijkstraRewarding` in
  `Redeemers.hs`, `RecentEra Dijkstra` in `Eras.hs`).
- **FR-COINSEL.** `cardano-coin-selection` at
  `176611048d5b9f33df19d106cd925d63a7858bb2` stays. Confirmed: its
  `cabal.project` has no ecosystem constraints; its `.cabal` has no
  cardano-ledger/ouroboros depends. If the solve proves otherwise,
  report, do not bump it here.

## Hard stops

HS-DESIGN (most likely dijkstra 0.3.0.0), HS-BEHAVIOUR, HS-OUTSIDE,
or old-vs-new value differs: file Q and park.

## Invariants

| ID | Severity | Observable |
|---|---|---|
| INV-45-INDEX-STATE | BLOCKING | FR-INDEX |
| INV-45-CONSTRAINTS | BLOCKING | freeze-pinned == pins as table |
| INV-45-TEST-PINS | BLOCKING | two `-test` packages still `==` pinned; versions recorded |
| INV-45-BOUNDS | BLOCKING | FR-BOUNDS |
| INV-45-CHAP | BLOCKING | FR-CHAP |
| INV-45-NIX-INDEX | BLOCKING | FR-NIX-INDEX |
| INV-45-BUILD | BLOCKING | FR-BUILD |
| INV-45-TEST | BLOCKING | FR-TEST |
| INV-45-ADAPT-MECHANICAL | BLOCKING | mechanical only; unpinned seed-like values hunted |
| INV-45-NO-OUTSIDE | BLOCKING | this clone only |
| INV-45-OLD-PINS-GONE | BLOCKING | `ouroboros-consensus ==3.0.1.0`, `cardano-crypto-class ==2.3.2.0`, `2026-03-26T20:21:33Z` absent |
| INV-45-DIJKSTRA-REPORT | BLOCKING | 0.3.0.0 surface delta named in receipt, or "unchanged" with a control |

Auditor: sanity only. No mutation campaign. No new suites except
the one seed-size-like pin if found. No tracked `.gitignore` for
`/gate.sh` or `.orch/`.
