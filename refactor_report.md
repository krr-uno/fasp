# funasp refactor report

> Historical snapshot — most suggestions below have since been applied; see
> git history for what landed.

**Date:** 2026-07-03
**Branch:** `jorge/agg-not`
**Scope:** readability and simplification review of `funasp/` (no behavior changes proposed unless noted). Suggestions are grouped by kind and roughly ordered by value; within each group the mechanical ones come first. Rough total: 150–200 lines can be deleted outright.

## 1. Delete dead code (mechanical, zero risk)

- **`funasp/util/ast.py`** carries ~113 commented-out lines: the old `AST_T` TypeVar block (lines 51–116), the `HeadBodyVisitor` class (157–188), `create_head_literal` (245–261), and a leftover comment block at the end of `function_arguments_ast` (337–341). Git history preserves them; delete.
- **`funasp/core.py`**: the commented `add_original_statement` (55–59) is the only "user" of the `original_statements` dict (43) — delete both. The `ignore_info` flag (44) is never set to `True` anywhere in `funasp/` or `tests/`, so the condition in `handle_log_message` (69–71) collapses to just the `logger is not None` check.
- **`funasp/core.py` `Library.__init__`**: `shared`, `slotted`, `log_level`, `message_limit` are stored as attributes (31–35) but never read again — they exist only to construct `ClingoLibrary`. Pass them straight through and drop the attributes; the object's real state (messages, processing statement) then stands out.
- **`funasp/symbol.py:46-48`**: commented-out alternative `return`.
- **`funasp/util/types.py:1`**: commented import.
- **`funasp/__main__.py:33`**: `"-v" in args or "--version" in args` is a no-op expression; the surrounding commented-out prints (26, 34–35, 38–39) should go with it. While there, fix the walrus precedence on line 36 (`if (error_code := check_versions()) != 0:` — see bug report E).
- **`funasp/app.py` `fasp_main`**: the `raise_errors` parameter is only consulted inside a `pragma: no cover` `except BaseException` block (148–151). Either wire it up meaningfully or remove the parameter.

## 2. Consolidate duplicated helpers

### 2.1 `transform_iterable` vs `map_none` — two names for one function

`funasp/ast/_core.py:74-99` (`transform_iterable`) and `funasp/util/iterables.py:4-22` (`map_none`) implement the identical "map, keep original on `None`, return `None` if nothing changed" pattern; the only difference is that `transform_iterable` threads a `Library` argument, i.e. `transform_iterable(lib, it, fn) == map_none(partial(fn, lib), it)`. Keep one (in `util/iterables.py`), update the two call sites of the other (`negated_literals.py`, the `funasp.ast` re-export), and the mental model shrinks by one concept.

### 2.2 Twin parse wrappers

`funasp/ast/_parsing.py`: `parse_string` (39–74) and `parse_files` (77–112) are line-for-line identical — including the four-line comment about saving `error_messages` — except for which `ast.parse_*` they call. Extract:

```python
def _parse(library: core.Library, parse: Callable[[Callable[[ast.Statement], None]], None]) -> list[Statement]:
    ...
```

and have both wrappers pass a one-line lambda/partial. Halves the file.

### 2.3 Twin control loaders

`funasp/control.py`: `parse_files` (38–59) and `parse_string` (61–82) share everything after the `parse_*` call — rewrite, build `ast.Program`, `join`, store `_rewritten_program`. Extract a private `_load(self, statements: list[Statement]) -> None` and each public method becomes two lines. This also guarantees the two paths can't drift (today a fix applied to one must be mirrored by hand).

### 2.4 Identical branches in `_rename_head`

`funasp/ast/_rewritings/prefixes.py:58-81`: the `HeadSetAggregate` and `HeadAggregate` branches are character-for-character identical. Merge into one `isinstance(head, ast.HeadSetAggregate | ast.HeadAggregate)` branch.

### 2.5 Re-derived constant

`funasp/ast/_rewritings/some_assignments.py:27` defines `SOME_PREFIX = PARSER_PREFIX + SOME_MARKER`, which already exists as `PARSER_SOME_PREFIX` in `funasp/ast/_core.py:31`. Import it instead.

### 2.6 Atom-name extraction is reimplemented three times

The "get the predicate name (and arguments/pool) out of a `TermFunction` *or* a function-typed `TermSymbolic`" dance appears in `_rewritings/collectors.py:29-41`, `util/collectors.py:43-55`, and `restore.py:28-34` (and half of it again in `comparisons.py:49-66`). A single `util` helper — e.g. `atom_name(atom) -> str | None` next to `function_arguments_ast` — would remove the copies and give one place to handle the symbolic/non-symbolic split (whose asymmetric handling caused report item A-class bugs like the missing `TermSymbolic` case in `prefixes.py`).

### 2.7 `report_error_summary`

`funasp/app.py:91-114`: two near-identical colored-print blocks. One `_print_error(message: str)` helper, two calls.

## 3. Simplify specific functions

### 3.1 `_functional_constraint` — the arity-0 branch is redundant

`funasp/ast/_rewritings/constraints.py:39-48`: for `arity == 0` the general branch already produces the same result (`args1 = [] + [_] == [_]`, `args2 = [] + [V] == [V]`). Delete the special case; the function drops to a single code path.

### 3.2 `StatementWeakConstraint` handler

`funasp/ast/_rewritings/unnesting.py:318-366`: the `comps_1`/`comps_2` split, the `are_new_body_literals` flag, and the `new_body_literals_from_comps` re-copy obscure a simple shape: unnest the tuple, unnest the body with `map_none`, then extend the body with all popped comparisons. Also, `update["body"]` is set unconditionally, so the statement is rebuilt even when untouched. A rewrite along these lines is ~15 lines shorter and matches the other handlers.

### 3.3 The three "element" handlers share one skeleton

`unnesting.py`: the handlers for `BodyAggregateElement`/`HeadAggregateElement` (169–199), `SetAggregateElement` (201–226), and `OptimizeElement` (228–253) all do: build a local `UnnestFunctionsInLiteralsTransformer(allowed_in_negated_literals=False)`, unnest tuple/literal/condition with `map_none`, then append popped comparisons to the condition. Extract a helper parameterized by which parts the element has (tuple? literal?), or at least the common "extend condition with leftovers" tail — the `condition = condition or list(node.condition)` idiom is currently copy-pasted three times.

### 3.4 `Model.to_str`

`funasp/solve.py:99-105`: the three-way return is

```python
return "\n".join(s for s in (predicate_str, function_str) if s)
```

### 3.5 `_build_intensional_function_to_term`

`funasp/ast/_rewritings/comparisons.py:42-73`: the `if`/`elif` without an `else` leaves `name`/`pool` unbound if neither matches (mypy tolerates it only because of the guard at the call site). Restructure with an early `assert`/`raise` or a final `else`, so the function is self-evidently total.

### 3.6 `normalize_log_message`

`funasp/core.py:75-99`: the magic slice `lines[0][9:-3]` and the literal `"undefined predicate F"` are the least self-explanatory lines in the package. Name the patterns (module-level regex constants with a comment showing a sample message) and take the prefix as a parameter instead of hardcoding `F` (this is also bug D2).

## 4. Clarify contracts (highest readability value)

### 4.1 Make "None means unchanged" hold everywhere in `unnesting.py`

The clingo transformer contract (return `None` for unchanged, a node otherwise) is applied inconsistently in `StatementUnnestTransformer._rewrite_literal`:

- the `HeadSimpleLiteral` register returns `node` when unchanged (261),
- the aggregate register always returns `node.update(...)` (162–167),
- the element registers always return `node.update(**update)` even when `update` is empty,
- the default returns whatever `node.transform` gives (which does follow the contract).

Consequences: the short-circuit `if not new_head and not are_new_body_literals: return node` (287) almost never fires yet reads as load-bearing; statements are rebuilt when untouched; and, more importantly, a reader cannot tell which handlers *intend* to signal "unchanged". Bugs like A1 (disjunction heads silently skipped) hide precisely in this ambiguity — the default handler looks like it recurses usefully, but never reaches the term-level logic. Suggestion: make every register honor the contract (return `None` when `update` is empty / nothing changed) and have the two `_rewrite` statement handlers rebuild only on change. This is the single most valuable readability refactor in the package.

### 4.2 Make `StatementUnnestTransformer` one-shot

`transform_statement` (44–59) assigns `self.head_literal_transformer` / `self.body_literal_transformer` outside `__init__`, making the class stateful across an implicit protocol (pylint would flag W0201, and a second `transform_statement` call silently reuses a fresh generator against stale expectations). Since `unnest_statement` constructs the object per statement anyway, move the `var_gen` and both literal transformers into `__init__` (constructed from the statement passed to a classmethod, or pass the statement to the constructor).

### 4.3 `Statement.rewrite`

`funasp/ast/_core.py:64-71`: move `self.rewritten = new_rewritten` out of the loop, and replace the two-callable union `_REWRITE_FUNCTION` (34–37) with the simpler `Callable[[ast.Statement], ast.Statement | list[ast.Statement]]` — the current union suggests a distinction that does not exist at call sites.

### 4.4 `RewriteContext` constructor

`funasp/ast/_rewritings/context.py:18`: `lib: Library = Library()` creates a shared clingo library at import time (bug E). Make `lib` required — every real call site passes it — and the class loses a footgun. While there, `fresh_predicate_name` (37–44) rebuilds `used_names` from `self.predicates` on every call; compute it lazily once or maintain it incrementally.

### 4.5 `restore.py`: is the `StatementRule` register needed?

`_RestoreNonIntensionalFunctionsTransformer` registers a manual head+body loop for `StatementRule` (94–119) that appears to re-implement exactly what the default `node.transform(self.library, self.dispatch)` (84–87) would do. If a test run with the register removed stays green, delete it (~25 lines). Related smell: the default `dispatch` is marked `pragma: no cover`, yet it must execute for heads and aggregates — either the pragma is wrong (remove it and let coverage prove the path) or the method truly never runs (then the class collapses further).

### 4.6 Type-alias sprawl

`funasp/util/ast.py:19-48` defines the `AST` union; `funasp/ast/_rewritings/literals.py:25-54` defines the overlapping `AST_T` TypeVar with its own 28-type list. Keeping both lists in sync by hand is busywork; derive one from the other or move both next to each other in `util/ast.py` with a comment explaining why the TypeVar variant exists (singledispatch registration constraints).

### 4.7 Docstrings inherited from clingo that are wrong here

- `funasp/solve.py` `Model` docstring claims "`Model` objects cannot be constructed from Python" — this class is constructed in `Control.solve`.
- `predicate_symbols` documents a `complement` parameter that does not exist (52–57).
- `Control.get_rewritten_program`'s `Returns` block is fine now, but `Control.solve` and `Control.ground` docstrings are also verbatim clingo text; trim to what this wrapper actually guarantees.

## 5. Smaller polish

- **`funasp/control.py:36,129`**: `self._result` is written but only consumed by `tests/test_app_patch.py` reaching into the private attribute. Expose a read-only `result` property and have the test use it.
- **`funasp/ast/__init__.py`**: exports `transform_iterable`; after 2.1 this re-export should point at (or be replaced by) the single surviving helper.
- **`funasp/app.py:66-67`**: printing the rewritten program in Rewrite mode inside `main` mixes UI with control flow that otherwise only builds state; a tiny `_run_rewrite_mode` method would make `main`'s try/except ladder read as pure error routing.

## Suggested order

1. Section 1 (deletions) — one commit, no test changes.
2. 2.1–2.5 and 3.1/3.4/3.5 — mechanical consolidations, existing tests cover them.
3. 4.1 + 4.2 (unnesting contract) — the most valuable and the one to do *before* fixing report bugs A1/A3, since those fixes land in the same handlers.
4. 2.6 (atom-name helper) — best done together with the A4/C1 prefix fixes, which touch the same seams.
