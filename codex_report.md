# funasp code review report

> Historical snapshot — this review predates the fixes it recommended (most
> have since landed) and refers to `CLAUDE.md`, which was consolidated into
> `AGENTS.md`. It is kept for engineering history, not as a status document.

## Executive summary

The current tree is a compact translation layer that turns FASP syntax into
ordinary clingo AST, solves the resulting program with clingo, and translates
internal function predicates back into user-facing assignments.

The core architecture is sound:

```text
FASP source
  -> clingo_funasp parser
  -> F/FS-prefixed clingo AST
  -> ordered rewrite pipeline
  -> ordinary clingo program
  -> grounding and solving
  -> user-facing predicates and f(args)=value assignments
```

The strongest parts are:

- Clear separation of rewriting concerns into individual modules.
- Exact-string integration tests that document the translation.
- Strong automated verification: 213 tests pass with 100% line coverage.
- Strict type checking passes.
- Recent work substantially improved prefix handling, error normalization,
  negated conditions, double negation, and internal-predicate hiding.

The main remaining risks are known unsupported AST shapes, assertion-based
failures for accepted syntax, inconsistent transformer contracts, shared
mutable default state, and documentation that no longer fully matches the
current layout.

## Architecture

### Parsing

`clingo_funasp` parses assignments into normal clingo AST using an internal
prefix:

- `f(t) := v` becomes `Ff(t,v)`.
- `f(t) := #sum{...}` becomes a guarded head aggregate.
- `#some` uses the special `FS` prefix.
- `#showf f/n` becomes an `Ff/(n+1)` show signature.
- Body equations such as `f(X)=V` are initially left as comparisons.

The parser-facing wrapper is `funasp/ast/_parsing.py`. Each parsed node is
wrapped by `Statement`, which retains both the original statement and its
current rewritten expansion.

### Rewriting

The pipeline in `funasp/ast/_rewritings/__init__.py` performs:

1. Predicate and `#showf` collection.
2. Function-prefix validation.
3. `#some` normalization.
4. Aggregate-assignment normalization.
5. Intensional-function signature collection.
6. Negated condition lifting.
7. Negated head/body rewriting.
8. Double-negation lifting.
9. Function-term unnesting.
10. Prefix renaming.
11. Function-equation conversion into prefixed predicates.
12. Native clingo AST rewriting.
13. Restoration of accidentally prefixed non-intensional signatures.
14. Addition of one uniqueness constraint per intensional function.

This ordering is semantically important. In particular, function signatures
must be known before unnesting, and restoration must happen after clingo
expands pools.

### Solving and presentation

`funasp/control.py` owns parse/rewrite/load and wraps models from clingo.
`funasp/solve.py` separates ordinary predicate symbols from encoded function
assignments and hides generated `RD`/`AD` auxiliaries. `funasp/symbol.py`
renders encoded atoms as `f(args)=value`.

## Current correctness risks

### 1. Intensional terms in disjunctive heads remain silently unsupported

The unnesting transformer handles `HeadSimpleLiteral`, aggregates, and
aggregate elements, but has no semantic handler for `HeadDisjunction` or
`HeadConditionalLiteral`. Its default traversal does not invoke the term-level
unnesting transformer.

The integration test explicitly records that disjunctions pass through
unchanged. Consequently:

```prolog
f(a) := 1.
p(f(a)) | q.
```

can retain `f(a)` as an ordinary Herbrand term instead of looking up the
intensional value.

This is the highest-priority issue because it can silently produce a wrong
answer set. Either implement the rewriting or reject such occurrences with a
`SemanticError`.

Relevant code: `funasp/ast/_rewritings/unnesting.py:61`.

### 2. Body set-aggregate guards are not explicitly handled

The aggregate registration covers:

```python
BodyAggregate | HeadAggregate | HeadSetAggregate
```

but not `BodySetAggregate`, at
`funasp/ast/_rewritings/unnesting.py:146`. Elements are handled separately,
but function terms appearing in a body set aggregate's guards may survive
unchanged.

This is another possible silent-semantic bug. It should receive a regression
test based on the existing `bug_report.md` reproduction.

### 3. Pooled aggregate assignments raise a raw assertion

`funasp/ast/_rewritings/aggregates.py:47` asserts that the assignment target
has exactly one pool entry:

```python
assert len(left.term.pool) == 1
```

The parser accepts pooled targets and `#some` already expands them correctly.
A pooled regular aggregate assignment therefore produces an internal
`AssertionError`. Assertions also disappear under optimized Python.

Recommended resolution:

- Expand each pool entry, matching `rewrite_some_assignments`; or
- Reject the construct with a source-located `RewritingException`.

### 4. Other statement forms bypass unnesting

`StatementUnnestTransformer._rewrite` only specializes rules, optimize
statements, and weak constraints. Intensional terms in constructs such as
`#external`, `#heuristic`, term-based `#show`, and `#edge` may remain ordinary
terms.

A capability matrix would help: list every clingo statement type and mark it
supported, intentionally passed through, or explicitly rejected.

### 5. Empty-prefix validation is inconsistent for empty programs

The pipeline documentation says an empty prefix is always rejected, but
`_validate_prefix_collisions()` returns immediately when no statements exist,
before checking the prefix (`funasp/ast/_rewritings/__init__.py:79`).

This has little practical effect for an empty program, but violates the
documented option contract. Check prefix emptiness before the empty-program
return.

### 6. Shared default `Library` instance

`funasp/ast/_rewritings/context.py:18` declares:

```python
lib: Library = Library()
```

That object is constructed at import time and shared by every context that
omits `lib`. `Library` contains mutable errors, function signatures, logging
state, and an underlying clingo library.

Every production caller currently supplies a library, but this remains a
latent isolation bug. Make `lib` mandatory or use `None` and construct it
inside `__init__`.

## Maintainability and refactoring opportunities

### Normalize the transformer contract

The intended convention is "return `None` when unchanged," but
`unnesting.py` does not follow it consistently:

- `HeadSimpleLiteral` returns the original node when unchanged.
- Aggregate and element handlers always rebuild nodes.
- The rule handler contains a no-change shortcut that is therefore rarely
  effective.
- Optimize and weak-constraint statements are rebuilt unconditionally.

This makes it difficult to distinguish actual transformations from traversal.
Normalize every handler to return `None` on no change.

### Simplify the weak-constraint rewrite

The weak-constraint handler maintains separate `comps_1`, `comps_2`,
`are_new_body_literals`, and `new_body_literals_from_comps` collections at
`funasp/ast/_rewritings/unnesting.py:325`. It can be reduced to:

- Rewrite the tuple.
- Rewrite body literals with `map_none`.
- Pop all generated comparisons.
- Append them to whichever body list is active.
- Rebuild only if something changed.

### Consolidate aggregate-element rewriting

`BodyAggregateElement`/`HeadAggregateElement`, `SetAggregateElement`, and
`OptimizeElement` repeat the same pattern:

- Create a local term transformer.
- Rewrite tuple/literal/condition.
- Append generated comparisons to the condition.

A small internal helper would reduce duplication and make differences between
element types explicit.

### Fix `Statement.rewrite`'s partial update

`self.rewritten = new_rewritten` is inside the loop at
`funasp/ast/_core.py:65`. Move it after the loop. The current code normally
works, but an exception midway leaves the wrapper partially updated.

The callable type can also be simplified to:

```python
Callable[[ast.Statement], ast.Statement | list[ast.Statement]]
```

### Merge duplicate iterable helpers

`transform_iterable` in `_core.py` and `map_none` in `util/iterables.py`
implement the same "replace only changed elements" abstraction. Keep a single
helper and use it consistently.

### Deduplicate parse and control loading paths

`parse_string` and `parse_files` in `_parsing.py` duplicate error-state
management. Likewise, `Control.parse_string` and `Control.parse_files`
duplicate the entire rewrite/load/store sequence in `control.py`.

Private `_parse(...)` and `_load(...)` helpers would prevent these paths from
drifting.

### Make `ComparisonTransformer` locally total

Pylint correctly notes that `name` and `pool` might be unbound in
`_build_intensional_function_to_term`. Its callers currently preserve the
necessary invariant, but the function's own contract is incomplete. Add a
final `else: raise TypeError(...)` or an assertion.

### Remove dead and misleading code

Examples include:

- The no-effect expression and commented code in `funasp/__main__.py:32`.
- Walrus precedence currently binds a Boolean rather than the actual error
  code:

  ```python
  if error_code := check_versions() != 0:
  ```

  Use:

  ```python
  if (error_code := check_versions()) != 0:
  ```

- Commented alternatives in `symbol.py`.
- Unused `sign` plumbing in `util/ast.py`.
- `original_statements` and related commented code in `core.py`.
- `Model` documentation inherited from clingo, including a nonexistent
  `complement` argument.

## Documentation review

### Strong documentation

- `CONTEXT.md` provides a valuable shared vocabulary.
- The ADRs clearly explain why flattening and prefix encoding were selected.
- `bug_report.md`, `bug_fixes.md`, `refactor_report.md`, and `review.md`
  preserve useful engineering history.
- `PROMPTS.md` documents the evolution of requirements and explains several
  otherwise surprising implementation choices.

### Documentation drift

Several Markdown files still reference the former `funasp/rewriting/` layout
instead of the current `funasp/ast/_rewritings/` package. Some named files,
such as `rewriting/integration.py`, no longer exist.

Other inconsistencies include:

- `AGENTS.md` describes `funasp/util/ast.py` as owning parsing wrappers and log
  normalization, while these now live in `_parsing.py` and `core.py`.
- `CLAUDE.md` refers to a nonexistent `_rewritings/rewrite_statements.py`.
- `bug_fixes.md` says multi-character CLI prefixes are rejected in one note,
  although current tests confirm they are accepted.
- `README.md` has unfinished or awkward sections, particularly "Choice
  assignments," and several spelling errors.
- The README would benefit from documenting partiality/undefinedness and
  explicitly listing unsupported syntactic positions.

The ADRs and `CONTEXT.md` should be treated as stable design documents.
`README.md`, `AGENTS.md`, and `CLAUDE.md` should be updated to match current
code, while bug and refactor reports should be clearly labeled as historical
snapshots.

## Test and tooling status

Verified locally:

- `nox -s test`: passed.
- 213 tests passed.
- Coverage: 100% across 1,375 measured statements.
- `nox -s typecheck`: passed under strict mypy.

Additional findings:

- The format session could not complete because `autoflake` attempted
  multiprocessing that the sandbox forbids. This was an environment
  restriction, not a reported formatting difference.
- `nox -s lint` fails. Some diagnostics are tool/configuration noise, but
  several are actionable:
  - No-effect statement in `__main__.py`.
  - Possibly unbound variables in `comparisons.py`.
  - Transformer attributes created outside `__init__`.
  - Dead arguments and built-in shadowing.
  - Long lines and missing exception chaining.
- The working tree already contained user changes to `PROMPTS.md` and an
  untracked `review.md`; the review did not modify them.

## Recommended priority

1. Fix or explicitly reject intensional functions in disjunctive heads.
2. Cover and fix `BodySetAggregate` guards.
3. Handle pooled aggregate assignments without assertions.
4. Audit every statement/head/body AST type and establish an explicit support
   matrix.
5. Remove the shared `Library()` default.
6. Normalize the unnesting transformer's `None means unchanged` contract.
7. Consolidate duplicate parsing/loading/iterable helpers.
8. Refresh `README.md`, `AGENTS.md`, and `CLAUDE.md` against the current package
   layout.
9. Repair the lint configuration and then address the remaining meaningful
   diagnostics.
