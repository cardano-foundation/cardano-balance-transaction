# Plan — cardano-balance-transaction#45

One OWNER slice. LIGHT ineligible: dijkstra 0.3.0.0 and two majors.

## Topology (exceptions, not precedent)

- TO: grok pane `%124` (`context=REUSED` from clr21)
- CO: `qwen --yolo --model qwen3.8-max-preview` `draft=NONE`
- Auditor: `claude --dangerously-skip-permissions --model sonnet --effort high`
- Push/PR: parent `%121`. Local `git -c commit.gpgsign=false`.

## cardano-deps mapping

s4 CHaP → `c0770200…`. s5 freeze already produced. s6 bounds.
s7 cabal.project. s8 N/A. Nix `indexState` + likely hackage snapshot
(`#21` Cabal-7159). No `cardano-lmdb` override here (control:
`nix/project.nix` has only crypto-praos/crypto-class pkgconfig).
Leave those and do not invent an lmdb deletion.

## Slice S1

Base: `6875d5b9702bcc0c4b4ebfffda118a705e70feed`.
Branch: `chore/issue-45-node-11-1-0`.
Worktree: this clone. Never `/code/cardano-balance-transaction`
or `/code/cardano-wallet`.

Owned: `cabal.project` (index-states + freeze constraint members +
solved `-test` pins), `cardano-balance-tx.cabal` bounds,
`flake.nix` indexState, `flake.lock` (CHaP + hackage snapshot as
#21), `nix/project.nix` only if eval rejects a stale
`packages.*` override, `lib/**/*.hs` `test/**/*.hs` compiler-forced,
one existing-suite assertion if an unpinned adaptation value is
found.

Forbidden: extra constraint members, invented versions, SRP bump,
tracked gitignore harness paths, other repos, GPG, push.

Build: repo `nix develop`, not a bare `nix shell`.
