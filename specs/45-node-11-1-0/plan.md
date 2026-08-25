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

Build: this repo's CI (`just build` / `just unit` = `nix build` /
`nix run` of flake packages). Not a bare `nix shell`. cabal-in-shell
is not the gate (Q1).

## Gate v3 (post-push correction, ticket-owner ruling)

CI failed `Fourmolu` on PR #46 after push: gate v2's fourmolu/hlint step
was scoped to `git diff --diff-filter=ACMR <base> -- '*.hs'` (only files
this slice touched), while CI's own `.github/workflows/ci.yml` runs
`fourmolu -m check lib test` / `hlint lib test` over the **whole tree**.
This slice's own T004 flake.lock haskellNix/hackage-snapshot bump moves
the fourmolu version the dev shell and CI both resolve (now 0.20.1.0),
so 17 files this slice never touched newly disagreed with the new
toolchain's layout opinion — a real CI failure the diff-filtered gate
structurally could not see. `main` at this branch's base was itself
CI-green (run 28228668216, 2026-06-26): not a pre-existing repo issue.

Gate v3 replaces the diff-filtered fourmolu/hlint step with the exact
whole-tree commands CI runs (`fourmolu -m check lib test`, `hlint lib
test`, plus `nixfmt --check flake.nix nix/*.nix` for completeness).
Negative control: v3's fourmolu step exits 100 against the pre-fix tree
(captured before the fix, 17-file diff). Fix: `fourmolu -i lib test`,
layout-only (insertions == deletions in every file) — kept as its own
commit (`style: fourmolu 0.20.1.0 whole-tree reformat`) stacked right
after the pin-set commit, rather than folded into it, so the pin-set
diff stays reviewable on its own. v3 hash:
`bde57aa638987b0523a376fe3e0b059df5670cde4e2cbf8bbc501da8a5351908`.
