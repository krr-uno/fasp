# funasp bug report

Open findings only — fixed items are removed once resolved; see the git
history for the original full report (first version 2026-07-03, branch
`jorge/agg-not`) and for how each fix landed.

**Scope:** full read of `funasp/` (rewrite pipeline, control/app, utils), with
each suspected bug verified by running the rewrite pipeline or the CLI in the
`funasp` conda env.

## E. Code-quality / latent issues (no observed misbehavior yet)

- **`Statement.rewrite`** (`funasp/ast/_core.py:64-71`): `self.rewritten =
  new_rewritten` sits *inside* the loop. It works only because the iterator was
  captured first; if `func` raises mid-loop, `rewritten` is left half-migrated.
  Move the assignment after the loop.
