# Two-axis code review — `a5cdcb6...HEAD`

**Date:** 2026-07-13
**Range:** `a5cdcb6` (codex pylint pass) → `aaa291a` (test_integration refactor); 8 commits, 36 files, +1920/−1563.
**Axes:** *Standards* (repo conventions in `AGENTS.md`/`CONTEXT.md`/`docs/support-matrix.md` plus a fixed Fowler smell baseline) and *Spec* (faithfulness to the session's requirements R1–R7). The axes are reported separately by design; findings are not reranked across them.

## Standards

1. **`docs/support-matrix.md` not updated despite classification changes** (AGENTS.md "Key Conventions" + the matrix's own maintenance rule: "Update it whenever a position becomes supported, rejected, or inapplicable"). The diff removes the `SemanticError` "intensional functions are not allowed in negated literals in optimization statements and aggregate element literals" (`funasp/ast/_rewritings/literals.py`) and implements translation for negated/double-negated intensional literals in optimize elements, weak-constraint bodies, and set/head-aggregate element literals — these positions moved rejected → supported, yet `docs/support-matrix.md` and `tests/rewriting/test_support_matrix.py` are untouched. Mitigating: the matrix's coarse rows already said "Supported", so it isn't now *wrong* — but the rows for `StatementOptimize`, `StatementWeakConstraint`, and the aggregates should note that negated occurrences are lifted. *Resolved (2026-07-13): the matrix intro and seven rows now describe the negated-occurrence lifting, and `test_support_matrix.py` gained an 11-case negated-position leakage matrix mirroring it.*
2. **ADR 0002 edited in place** — AGENTS.md says "do not silently rewrite their decisions". The edits are a stale-path fix and a typo; decisions unchanged, so acceptable, noted for the record.

Baseline smells (all judgement calls):

- **Speculative Generality** — `literals.py`: the `sign` parameter of the term-unnesting register is now dead, kept via `# pylint: disable=unused-argument`, and `unnest_functions` still threads it through. Delete the parameter chain rather than suppress the lint. *Resolved (2026-07-13): the parameter was removed from `unnest_functions`, all five dispatch registers, and every internal `transform` call.*
- **Duplicated Code** — `negated_literals.py`: the `StatementOptimize`/`StatementWeakConstraint` branches of `rewrite_negated_condition_literals` share the exact shape, as do the `BodyAggregate`/`BodySetAggregate` branches of `_rewrite_body_element`. Extractable. *Partially resolved (2026-07-13): the aggregate pair in `_rewrite_body_element` was merged into one branch that varies only in the element rewriter, with a comment naming the real difference. The statement-level pair was deliberately left duplicated: abstracting over which attribute to read and rebuild costs `getattr`/dynamic-kwargs readability for ~8 lines saved.*
- **Data Clumps** — the `(context, auxiliary, library)` triple travels through ~9 functions; `library` is always `context.lib.library`. A small "lifter" object would absorb it (pre-existing pattern the diff extends).
- **Mysterious Name** — `tests/integration/base.py` carried over `self.elib` and the placeholder docstring `"""Assert transform equal."""` verbatim; the refactor was the moment to name them honestly.

## Spec

### (a) Missing / partial

- **R2 (deviated):** `bug_fixes.md` and `bug_report.md` were updated as asked but then **deleted** in a later commit (`78dc8dc`). The agreed end state was open-items-only files stating there are none; at HEAD they don't exist, which also erases R1(a)'s status updates.
- Everything else verified: R1(b–f), R3 (fix + regression test), R4 (canonical example pinned verbatim), R7 (all 63 unique old tests present in `tests/integration/`, shadowed pool test resurrected with its original body).

### (b) Not asked for

- The file deletions above; the dead `sign` parameter papered over with a pylint disable; the repurposed app tests and new `disjunctive_head.lp` (reasonable, coverage-driven); the codex commits' interim `not RD` body encoding was fully superseded by the spec'd `not not RD` scheme — no surviving unrequested semantics.

### (c) Implemented but wrong

- **R5/R6 — the removed rejection may not have been unreachable.** Lifting only handles symbolic literals; negated **comparison** literals with nested intensional functions (e.g. `:~ p(X), not g(f(X)) = 1.`, or the same shape in aggregate-element conditions) now take the inline encoding with a **positive** definedness atom (`Ff(X,FUN); g(FUN)!=1`) — when `f(X)` is undefined the original negation holds but the encoding never fires, unlike rule bodies which correctly produce `#false: g(FUN)=1, Ff(X,FUN)`. No test pins this case.
- Minor: `_contains_intensional_functions` runs a throwaway full unnest per negated literal — correctness-neutral but wasteful.

**Aggregator's note on (c):** a pre-change probe of `:~ p(X), not f(X)+1 = 3.` on the *old* code also did **not** reject — so the mis-encoding of negated comparisons in these contexts may predate this diff (pre-existing bug) rather than be a regression from removing the rejection. Either way the semantic gap is real and untested; it needs a repro on `a5cdcb6` to classify, then either comparison lifting or a reinstated rejection for that path.

> **Resolved (2026-07-13):** classified as **pre-existing** — a worktree probe on `a5cdcb6` produced the identical wrong encoding (the removed rejection never fired for comparisons; their sign was never propagated to the term-level check). Fixed by lifting singly negated comparisons that need unnesting, with sibling non-negated literals copied as safety guards (`not f(X)+1 = 3` → `not RD1(X)` + `RD1(X) :- p(X); Ff(X,FUN); 1*FUN+1=3.`), across weak-constraint bodies, optimize conditions, aggregate/conditional-literal conditions, and comparison element literals. Plain functional equalities and top-level rule-body comparisons keep their existing correct paths. A follow-up extended the guards with the enclosing rule's non-negated simple body literals, so comparison variables bound outside the condition (e.g. `:- q(Y), 0 < #count{ X : p(X), not f(X)+Y = 3 }.`) stay safe in the auxiliary; sibling aggregates are deliberately excluded as guards to avoid the auxiliary depending on its own lifted condition. Pinned by 9 unit, 3 integration, and 2 end-to-end regression tests.

## Summary

**Standards:** 2 documented-standard findings + 4 smells; worst: support-matrix not synced with the rejected→supported reclassification. **Spec:** 1 deviation + 1 correctness finding; worst: negated intensional *comparisons* in weak/optimize/aggregate conditions silently mis-encode under undefined functions (regression or pre-existing — unclassified). Suggested starting points: the comparison gap and the support-matrix sync.
