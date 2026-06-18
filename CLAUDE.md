# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`funasp` extends **clingo 6** with intensional functions defined by the user. The headline syntax is the assignment rule:

```prolog
f(t1) := t2 :- Body.                 % deterministic assignment
{ f(t1) := t2 } :- Body.             % choice assignment
f(t1) := #sum{ X : p(X) } :- Body.   % aggregate assignment
color(X) := #some{r;g;b} :- country(X).
```

funasp parses this superset of the ASP language with the `clingo_funasp` parser, rewrites the function-specific constructs into semantically correct plain ASP, then hands them to clingo to ground and solve. Models are printed back with the function syntax restored.

## The FASP encoding (key to everything)

The `clingo_funasp` parser desugars assignments **at parse time** into ordinary clingo AST using a name-prefix encoding (purely syntactic — no semantics):

| FASP source | Parsed as |
|---|---|
| `f(t) := v :- B.` | `Ff(t,v) :- B.` — `F`-prefixed atom, value appended as last argument |
| `{ a := 1 } :- B.` | `{ Fa(1) } :- B.` (`HeadSetAggregate`) |
| `f(X) := #agg{…} :- B.` | `Ff(X) = #agg{…} :- B.` (`HeadAggregate` left guard, **no** value slot) |
| `c(X) := #some{…} :- B.` | `FSc(X) = #sum{…} :- B.` (**`FS`** prefix marks `#some`) |
| `#showf p/1.` | `#show Fp/2.` (arity+1) |
| body occurrences | **untouched** — `b :- a = 1.` keeps the plain comparison |

The encoding is unambiguous because user-written function names cannot start with an uppercase letter. The semantic work (unnesting, uniqueness constraints, `#some` semantics, body equalities) is done by `funasp/ast/_rewritings/` — grounding the parsed output directly gives wrong answers.

## Environment

Requires **Python ≥ 3.13** and **clingo-funasp ≥ 6.0.0** (a clingo 6 fork with the FASP parser, from test.pypi via `requirements.txt`). Setup (conda):

```bash
conda create -n funasp python=3.13
conda activate funasp
pip install -r requirements.txt
pip install -e .
funasp examples/family.lp
```

## Build, test, lint

All dev tasks run through **nox**, configured with `default_venv_backend = None` so sessions run in the *current* env (no fresh venv, no auto-install):

| Command | What it does |
|---|---|
| `nox -s test` | Unit tests + **100% coverage** check; excludes the slow `tests/test_app.py` |
| `nox -s ftest` | Fast tests only — also excludes the integration tests (`test_app_patch.py`, `test_control.py`) |
| `nox -s slow_test` | Full suite via `unittest discover`, including integration tests |
| `nox -s typecheck` | `mypy --strict` on the `funasp` package |
| `nox -s format` | autoflake + isort (black profile) + black, in-place; pass `-- check` to only diff |
| `nox -s lint` | pylint on `funasp` |

Default sessions are `typecheck, test, format` locally and `typecheck, slow_test, format` in CI.

Run a single test file directly with unittest:

```bash
python -m unittest tests.rewriting.test_integration -v
# or a single test case:
python -m unittest tests.rewriting.test_integration.TestRewriteStatements.test_fibo -v
```

**100% line coverage on `funasp/` is enforced** (`--fail-under=100`). New code paths need tests; use `# pragma: no cover` / `nocoverage` for genuinely unreachable lines (`assert` lines are already excluded).

## Architecture

The pipeline is **parse → rewrite → clingo ground/solve → print**. Three entry layers:

- `funasp/__main__.py` — CLI entry point (`funasp` script). Validates Python/clingo versions, then calls `app.main`.
- `funasp/app.py` — `FaspApp(clingo.app.App)`. Registers funasp CLI flags (`--order`, `--prefix-fun`), drives parse + solve, and formats errors. `fasp_main` wraps everything in a `funasp.core.Library` context.
- `funasp/control.py` — `Control`, the funasp-aware analogue of `clingo.Control`. `parse_files`/`parse_string` call the `funasp.ast` parse wrappers + the rewrite pipeline and `join` the resulting clingo AST into the underlying clingo control. Also retains the rewritten program string (`get_rewritten_program`, shown in clingo's Rewrite mode).

Parsing itself is `funasp.ast.parse_string`/`parse_files`: thin wrappers over the callback-based `clingo_funasp.ast.parse_string`/`parse_files` that return a `list[funasp.ast.Statement]` and convert errors into `ParsingException` with `SyntacticError` locations. `funasp.ast.Statement` is a small dataclass bundling each parsed statement's `original` clingo AST with the `rewritten` clingo statements it expands to (`rewritten` starts as `[original]`); the rewrite pipeline fills `rewritten` in place.

### The rewrite pipeline (the core of the project)

`funasp/ast/_rewritings/` turns the parser's syntactic F-encoding into a semantically correct program. Everything is orchestrated by **`rewrite_statements(context, statements)`** in `_rewritings/rewrite_statements.py` — read this first; it is the spine of the system. Two passes over plain clingo statements:

1. Per statement: `rewrite_some_assignments` (`some_assignments.py`, `FS` aggregate → choice `= 1` + `#count ≥ 1` body) → `rewrite_assignment_aggregates` (`aggregates.py`, `Ff(X) = #agg{…}` → `Ff(X,W)` head + body aggregate) → `rewrite_negated_body_literals` (`negated_literals.py`, `not l` → `#false : l`), collecting intensional-function signatures (`collect_intensional_function_signatures` in `collectors.py`, from prefixed head atoms: arity−1). All pass-1 steps detect the parser's hardcoded `F`/`FS` directly.
2. Per statement: `unnest_statement` (`unnesting.py` driver + `literals.py` term logic: nested intensional `f(t)` → fresh `FUN` var + `f(t)=FUN` comparison) → `rename_prefixes` (`prefixes.py`, applies `--prefix-fun` by renaming the parser's hardcoded `F`/`FS`) → `prefix_comparisons` (`comparisons.py`, intensional `f(t)=v` → `Ff(t,v)`, pools handled) → clingo's own `ast.rewrite_statement` → `restore_non_intensional_functions` (`restore.py`, un-prefixes unpooled entries whose arity is not intensional). Finally `functional_constraints` (`constraints.py`) appends one uniqueness constraint `:- Ff(X…,_), 1 < #count{V: Ff(X…,V)}.` per intensional function.

Shared state lives in `RewriteContext` (`_rewritings/context.py`): the `funasp.core.Library`, the function-name prefix, the clingo `RewriteContext`, and the accumulated set of `SymbolSignature`s (`types.py`).

- `funasp/core.py` — `Library` (a wrapper around clingo's `Library` that captures/normalizes log messages — e.g. "undefined predicate F…" → "undefined intensional function …" — and carries the `processing_statement` text used in error reports).
- `funasp/ast/` — the `ast` package. `__init__.py` re-exports the public API; `_core.py` holds the `Statement` wrapper, the `transform_iterable` AST-iteration helper, the parser-prefix constants (`PARSER_PREFIX`, `SOME_MARKER`, `PARSER_SOME_PREFIX`), and `_ast_to_str` (re-prints an as-parsed F-encoded statement back in FASP syntax for error/info messages); `_parsing.py` holds the parse wrappers (`parse_string`/`parse_files`); `_rewritings/` holds the rewrite pipeline.
- `funasp/util/ast.py` — AST helpers (`create_literal`, `is_function`, `FreshVariableGenerator`, `ParsingException`, `SyntacticError`).
- `funasp/solve.py` / `funasp/symbol.py` — `Model` wrapper that re-renders `Ff(t,v)` atoms as `f(t)=v` in output, and symbol helpers.

### The function prefix

Rewritten function predicates carry a prefix (default **`F`**, set via `--prefix-fun`). The parser hardcodes `F`/`FS`; `prefixes.py` renames to the configured prefix right after parsing. The `Model` wrapper strips the prefix so output shows function syntax rather than the internal predicates.

## Conventions

- Each rewriting concern is its own module under `funasp/ast/_rewritings/`; wire new steps into the pipeline in `rewrite_statements.py`, not inline.
- Transformers follow the clingo AST visitor contract: `singledispatchmethod` + `node.transform(lib, fn, …)` returning `None` for "unchanged" and a new node otherwise; rebuild via `node.update(lib, **changes)`.
- Integration tests (`tests/test_app.py`, `test_app_patch.py`, `test_control.py`) are slow and excluded from the fast sessions; pipeline unit tests live under `tests/rewriting/`, parser tests under `tests/parser/`.
- `tests/rewriting/test_integration.py` asserts exact rewritten-program strings; when adding pipeline behavior, capture the actual output with a probe script first, sanity-check it, then hardcode it.
- Code style is enforced by `nox -s format` (autoflake + isort black-profile + black).

## Examples

`examples/*.lp` are funasp encodings (graph coloring, sudoku, hamiltonian, king, blocks, coins, …); `examples/pure_asp/` holds plain-ASP counterparts. They double as manual smoke tests: `funasp examples/<name>.lp` — and `tests/examples.py` pins their expected models.
