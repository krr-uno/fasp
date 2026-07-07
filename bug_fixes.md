# FUNASP Bug Fix Notes

This file tracks fixes and decisions while working through `bug_report.md`.

## A. Wrong answer sets

### A1. Intensional functions in disjunctive heads

Status: known unsupported case.

`funasp` does not currently support `HeadDisjunction` rewriting for intensional
functions. Treat this as a known limitation until support or a clear diagnostic
is added.

### A2. Conditional body literal equation leak

Status: already fixed.

No new changes were needed. The bug report already marks this as fixed.

### A3. Body set aggregate guards are not unnested

Status: TBD.

Need to decide whether to support unnesting `BodySetAggregate` guards or reject
them with a clear rewriting error.

### A4. Invalid `--prefix-fun` corrupts user predicates

Status: fixed.

Changes made:

- Removed the earlier one-uppercase-letter restriction because multi-character
  prefixes such as `__csp_` must be allowed.
- After used predicates are collected by the rewriting pipeline, reject prefixes
  that collide with predicate names in the program.
- Add `--ignore-prefix-collisions` to bypass the collision check when requested.
- Keep function-symbol rendering prefix-length-aware for multi-character
  prefixes.

## B. Crashes on accepted input

### B1. Pooled aggregate assignments crash

Status: TBD.

Need to decide whether pooled aggregate assignments should be expanded like
`#some` assignments or rejected with a proper `RewritingException`.

### B2. `Control.get_rewritten_program` before parsing

Status: already fixed.

No new changes were needed. The bug report already marks this as fixed.

## C. Wrong or leaking output

### C1. Multi-character prefixes garble model output

Status: fixed.

Changed function-symbol rendering to trim by the configured prefix length:

```python
FunctionSymbol.from_symbol(symbol, prefix_len=len(self.prefix))
```

This keeps `FunctionSymbol.from_symbol` prefix-aware and fixes the old
single-character assumption.

Note: A4 now rejects multi-character CLI prefixes, but the display code remains
correct for any internal or future caller that provides a longer prefix.

### C2. Auxiliary `RD*` predicates leak into answer sets

Status: fixed.

Auxiliary predicates introduced while lifting negated condition literals are
hidden from model output.

Changes made:

- Add display-layer filtering for internal auxiliary predicates with the `RD`
  prefix family.
- Generate alternate auxiliary prefixes when `RD` would collide with the
  configured function prefix, for example with `--prefix-fun=R`.
- Keep the filter in `funasp.solve`, where predicate and function symbols are
  converted into user-facing model output.
- Add an end-to-end regression test for the C2 repro.

## D. Error-reporting quality

### D1. Double-negated literals over intensional functions report unsafe `FUN`

Status: fixed.

Double-negated literals over intensional functions are supported by keeping the
generated function lookup positive.

Decision for review:

- The bug report identified two possible directions: either support this
  construct with a positive definedness atom, or reject it with a clear semantic
  error that does not expose internal variables.
- Current decision: support the construct. The generated function equation is
  treated as a positive binding/definedness atom, while the user-written literal
  keeps its double negation.

Reasoning:

- The user literal keeps its double negation, for example `not not p(FUN)`.
- The generated function equation is an internal binding/definedness literal,
  so it must stay positive, for example `Ff(a,FUN)`.
- If the generated equation is also double-negated, `FUN` has no positive
  occurrence and clingo reports the internal variable as unsafe.

Tests:

- `tests/rewriting/test_end_to_end.py`
  `TestEndToEnd.test_double_negated_intensional_function_literal_can_bind`
- `tests/rewriting/test_end_to_end.py`
  `TestEndToEnd.test_double_negated_intensional_function_literal_can_fail`

### D2. Log normalization hardcodes the default prefix

Status: fixed.

Undefined-intensional-function log normalization now uses the configured
function prefix instead of hardcoding `F`.

Changes made:

- Store the active function prefix on `Library`.
- Set `Library.prefix_function` from `Control` and `RewriteContext`.
- Normalize `undefined predicate <prefix>...` messages into
  `undefined intensional function ...`.
- Suppress `<functional>` undefined-predicate messages for the configured
  prefix.
- Add unit coverage for custom prefix `G`.
