# funasp bug report

**Date:** 2026-07-03
**Branch:** `jorge/agg-not` (HEAD `c15e30b`)
**Scope:** full read of `funasp/` (rewrite pipeline, control/app, utils), with each suspected bug verified by running the rewrite pipeline or the CLI in the `funasp` conda env. Findings are ordered by severity.

## A. Wrong answer sets (silent semantic bugs)

### A1. Intensional functions in disjunctive heads are never unnested

`StatementUnnestTransformer._rewrite_literal` has no case for `HeadDisjunction` / `HeadConditionalLiteral`; the default handler (`funasp/ast/_rewritings/unnesting.py:61-71`) just recurses with `_rewrite_literal` itself, which contains no term-level replacement logic, so it can never reach `UnnestFunctionsInLiteralsTransformer.unnest`. The nested term survives to the ground program as an uninterpreted Herbrand term.

**Repro:**

```prolog
f(a) := 1.
p(f(a)) | q.
```

produces the model `p(f(a)) f(a)=1` — the literal atom `p(f(a))` instead of `p(1)`. The rewritten program keeps `p(f(a)); q.` untouched.

**Fix direction:** either unnest these heads like simple heads, or reject them with a semantic error; the current silent pass-through is the worst option.

### A2. Unnesting the main literal of a conditional body literal leaks the equation into the rule body

**Status: fixed** — the main literal is now unnested with a local transformer and its comparisons are appended to the conditional literal's condition.

In the `BodyConditionalLiteral` handler (`funasp/ast/_rewritings/unnesting.py:113`), `node.literal` is unnested with `self.body_literal_transformer`, whose generated comparisons are later appended to the *rule body* (`unnesting.py:293-294`). Variables that are local to the conditional literal escape and become global, changing the semantics from "for all X" to "there exists an X".

**Repro:**

```prolog
q(1). q(2).
f(1) := 3.
f(2) := 4.
p(3).
r :- p(f(X)) : q(X).
```

rewrites to `r :- Ff(X,FUN); p(FUN): q(X).` and derives `r`, although `p(f(2)) = p(4)` does not hold — `r` must not be derived.

**Fix direction:** the equation belongs inside the condition: `r :- p(FUN) : q(X), Ff(X,FUN).`

### A3. Guard terms of body set aggregates are never unnested

The aggregate handler at `funasp/ast/_rewritings/unnesting.py:139-144` registers `BodyAggregate | HeadAggregate | HeadSetAggregate` but not `BodySetAggregate`. Its elements are still handled (via the `SetAggregateElement` register), but its guards fall through the default recursion and stay untouched.

**Repro:**

```prolog
f(a) := 1.
p(1).
r :- f(a) { p(X) }.
```

rewrites to `r :- f(a) <= #count { 0,p(X): p(X) }.` with the literal term `f(a)` as guard; the comparison is decided by clingo's term ordering, so `r` is silently not derived even though `f(a)=1` and the count is 1.

### A4. An unvalidated `--prefix-fun` silently corrupts user predicates

`FaspApp._set_prefix` (`funasp/app.py:51-53`) accepts any string. The restore pass (`funasp/ast/_rewritings/restore.py:37-48`) un-prefixes *every* literal whose name starts with the configured prefix and whose stripped signature is not intensional, so a lowercase (or empty) prefix collides with user predicates.

**Repro:** `funasp lowpfx.lp --prefix-fun=go` on

```prolog
g := 1.
good(a,b).
```

rewrites the fact `good(a,b).` into the comparison head `od(a)=b.` and reports **UNSATISFIABLE** for a trivially satisfiable program.
Also, for `g := 1`, the answer set should have `g(1)` but it produces `og(1)` instead. This is likely due to how the added prefix is removed from the answer set in the final step to get back answer sets. It probably assumes a prefix of one character length and only removes the first character so `Gog(1)` becomes `og(1)`. This is also reported in `C1`.

**Fix direction:** validate the prefix (non-empty, one uppercase letter) at option-parsing time. Or validate the prefix (non-empty, starts with an uppercase letter) at option-parsing time and when getting creating answer sets, prefix aware trimming. Forcing the acceptance of a single letter uppercase is an easier fix.

## B. Crashes on accepted input

### B1. Pooled aggregate assignments crash with an `AssertionError`

`rewrite_assignment_aggregates` asserts unpooled left terms (`funasp/ast/_rewritings/aggregates.py:47`). The parser accepts pooling, and `#some` assignments explicitly support it (`some_assignments.py` emits one statement per pool entry), so

```prolog
c.
f(a;b) := #sum{ 1 : c }.
```

dies with `AssertionError: Terms must be unpooled Ff(a;b)` and a raw traceback. Either expand pools the way `#some` does, or raise a proper `RewritingException`. Note asserts also vanish under `python -O`.

### B2. `Control.get_rewritten_program` raises `AttributeError` before parsing

**Status: fixed** — `_rewritten_program` is initialized to `None` and the getter raises `ValueError` when no program has been parsed.

`self._rewritten_program: Optional[str]` at `funasp/control.py:35` is a bare annotation — the attribute is never assigned, so the `is None` check at `funasp/control.py:151` raises `AttributeError: 'Control' object has no attribute '_rewritten_program'` instead of taking the intended fallback. Initialize it to `None`.

Relatedly, `funasp/control.py:152-153` has an unreachable `raise` after a `return`, and the "not parsed yet" case returns a sentinel string rather than raising — pick one behavior.

## C. Wrong or leaking output

### C1. Multi-character prefixes garble model output

`FunctionSymbol.from_symbol` hardcodes `prefix_len: int = 1` (`funasp/symbol.py:24`), and `Model.function_symbols` (`funasp/solve.py:74-79`) never passes the actual prefix length despite knowing `self.prefix`.

**Repro:** `funasp prefix.lp --prefix-fun=Fun` on `f := 1.` prints `unf=1` instead of `f=1`.

### C2. Auxiliary `RD*` predicates leak into answer sets

The negated-condition lifting (`funasp/ast/_rewritings/negated_literals.py:88-116`) introduces `RDi` predicates but nothing hides them: `Model` only filters prefix-named atoms.

**Repro:**

```prolog
p(1). q(1). q(2).
r :- q(X) : not p(X), q(X).
```

prints `RD1(1) p(1) q(1) q(2) r`. These internal atoms should be filtered from output the same way `F`-atoms are (and considered in `#show` handling).

## D. Error-reporting quality

### D1. Double-negated literals over intensional functions fail with an internal unsafe-variable error

```prolog
f(a) := 1.
p(1).
b :- not not p(f(a)).
```

fails with `unsafe variables … FUN` — an internal variable the user never wrote. Cause: the double-negation path (`funasp/ast/_rewritings/unnesting.py:80-84`) generates the equation with `Sign.Double` via `make_equation` (`funasp/util/ast.py:544-558`), yielding `not not Ff(a,FUN)` where `FUN` is never positively bound. If this construct is meant to be supported, the equation needs a positive definedness atom; if not, it should be rejected with a clear semantic error like the aggregate-literal case. Given the branch is about aggregates/negation, this one is probably closest to the current work.

### D2. Log normalization hardcodes the default prefix

`Library.normalize_log_message` matches `"undefined predicate F"` literally (`funasp/core.py:93-98`), so with `--prefix-fun=G` the "undefined intensional function" rewording (and the `<functional>` suppression) silently stops working.

## E. Code-quality / latent issues (no observed misbehavior yet)

- **Shared default `Library()`**: `RewriteContext.__init__` uses `lib: Library = Library()` (`funasp/ast/_rewritings/context.py:18`) — the default is created once at import time and shared by every default-constructed context, including its mutable `error_messages` and `processing_statement` state. Use `None` + create-in-body.
- **`funasp/__main__.py:36`**: `if error_code := check_versions() != 0:` binds the *boolean* (`:=` has lower precedence than `!=`), so the function would return `True` rather than the actual error code; today that still exits 1, but it breaks the moment `check_versions` returns anything else. Line 33 (`"-v" in args or ...`) is a no-op expression left over from a refactor.
- **`Statement.rewrite`** (`funasp/ast/_core.py:64-71`): `self.rewritten = new_rewritten` sits *inside* the loop. It works only because the iterator was captured first; if `func` raises mid-loop, `rewritten` is left half-migrated. Move the assignment after the loop.
- **Statements not covered by unnesting**: `_rewrite` handles only rules, optimize statements, and weak constraints; intensional functions inside `#external`, `#heuristic`, `#show <term> : body`, or `#edge` statements pass through untouched — same silent-wrong-term class as A1, just in rarer positions.

## Suggested priorities

A1/A2/A3 are the core correctness bugs (wrong models, no diagnostics) and all live in `unnesting.py`; A4+C1 are one prefix-validation fix plus one plumbing fix; B1 sits directly in the aggregate code this branch is touching. A2 and B2 are the most mechanical fixes; A1/A3 need a decision on whether disjunction heads and set-aggregate guards should be supported or rejected.
