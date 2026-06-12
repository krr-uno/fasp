# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`funasp` extends **clingo 6** with evaluable (intensional) functions defined by the user. The headline syntax is the assignment rule:

```prolog
f(t1) := t2 :- Body.                 % deterministic assignment
{ f(t1) := t2 } :- Body.             % choice assignment
f(t1) := #sum{ X : p(X) } :- Body.   % aggregate assignment
color(X) := #some{r;g;b} :- country(X).
```

funasp parses this superset of the ASP language, rewrites the function-specific constructs down to ordinary clingo AST, then hands them to clingo to ground and solve. Models are printed back with the function syntax restored.

> Note: `AGENTS.md` covers the same ground but predates the `syntax_tree/` → `fun_ast/` package rename — trust the layout in this file and the actual tree when they disagree.

## Environment

Requires **Python ≥ 3.13** and **clingo ≥ 6.0.0** (a dev/preview build). Setup (conda):

```bash
conda create -n clingo6 python=3.13
conda activate clingo6
conda install -c potassco/label/dev-20 -c conda-forge clingo
pip install -r requirements.txt   # pulls tree-sitter-fasp from git
pip install -e .
funasp examples/family.lp
```

`requirements.txt` installs the **`tree-sitter-fasp`** grammar from `github.com/krr-uno/tree-sitter-fasp` — the parser depends on it.

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
python -m unittest tests.syntax_tree.rewriting.test_integration -v
# or a single test case:
python -m unittest tests.syntax_tree.rewriting.test_aggregates.TestClass.test_method -v
```

**100% line coverage on `funasp/` is enforced** (`--fail-under=100`). New code paths need tests. `funasp/fun_ast/rewritings/protecting.py` is the only source file omitted from coverage; use `# pragma: no cover` / `nocoverage` for genuinely unreachable lines.

## Architecture

The pipeline is **parse → rewrite → clingo ground/solve → print**. Three entry layers:

- `funasp/__main__.py` — CLI entry point (`funasp` script). Validates Python/clingo versions, then calls `app.main`.
- `funasp/app.py` — `FaspApp(clingo.app.App)`. Registers funasp CLI flags (`--order`, `--prefix-fun`), drives parse + solve, and formats errors. `fasp_main` wraps everything in an `ELibrary` context.
- `funasp/control.py` — `Control`, the funasp-aware analogue of `clingo.Control`. `parse_files`/`parse_string` run the parser + rewrite pipeline and `join` the resulting clingo AST into the underlying clingo control. Also retains the rewritten program string (`get_rewritten_program`, shown in clingo's Rewrite mode).

### The rewrite pipeline (the core of the project)

`funasp/fun_ast/` holds the funasp AST and all rewriting. Everything is orchestrated by **`rewrite_statements(context, statements)`** in `fun_ast/rewritings/integration.py` — read this first; it is the spine of the system.

- `_nodes.py` — funasp AST node types (`AssignmentRule`, `ChoiceAssignment`, `HeadSimpleAssignment`, `HeadAssignmentAggregate`, `ShowFDirective`, …). They mirror the clingo AST interface (`to_dict`, `update`, `visit`).
- `_context.py` — `RewriteContext`, shared state threaded through every rewrite step (the `ELibrary`, the function-name prefix, and the accumulated set of evaluable-function signatures).
- `types.py` — `SymbolSignature`.
- `collectors.py` — gathers evaluable-function signatures from statements.
- `parsing/parser.py` — uses `tree_sitter_fasp` to parse funasp-specific syntax, parses the rest with clingo, and **merges the two AST streams ordered by source location** (`_ast_merge`).
- `rewritings/` — one module per transformation step. `integration.py` applies them in two passes:
  1. Per statement: `rewrite_showf` → `rewrite_some_choices` → `normalize_assignment_aggregates` → `rewrite_negate_body_literals`, collecting evaluable-function signatures along the way.
  2. Per statement: `unnest_ast` (unnesting/) → `to_asp` (funasp AST → clingo AST) → clingo's own `ast.rewrite_statement` → `restore_non_evaluable_functions`. Finally `functional_constraints(context)` appends the constraints enforcing functionality.

`RewritingStatement` (in `integration.py`) wraps each statement and tracks whether it is currently funasp-level (`rewritten`) or clingo-level (`clingo_rewritten`); `to_asp` is the one-way switch between the two.

- `funasp/util/ast.py` — `ELibrary`, a ctypes wrapper around the clingo shared library, plus AST helpers (`create_literal`, `is_function`, `ParsingException`, `SyntacticError`). The whole CLI runs inside an `ELibrary` context manager.
- `funasp/solve.py` / `funasp/symbol.py` — `Model` wrapper that re-renders function predicates with the funasp prefix, and symbol helpers.

### The function prefix

Rewritten function predicates are emitted with a prefix (default **`F`**, set via `--prefix-fun`). The `Model` wrapper strips/reformats this prefix so output shows the original function syntax rather than the internal predicates.

## Conventions

- funasp AST nodes subclass `AssignmentAST` and follow the clingo AST contract — add new constructs the same way, and make `to_asp.py` handle them (it dispatches via `singledispatchmethod`).
- Each rewriting concern is its own module under `rewritings/`; wire new steps into the pipeline in `integration.py`, not inline.
- Integration tests (`tests/test_app.py`, `test_app_patch.py`, `test_control.py`) are slow and excluded from the fast sessions; fast unit tests for rewriting live under `tests/syntax_tree/rewriting/`.
- Code style is enforced by `nox -s format` (autoflake + isort black-profile + black). The parser module (`parsing/parser.py`) is `# mypy: ignore-errors`.

## Examples

`examples/*.lp` are funasp encodings (graph coloring, sudoku, hamiltonian, n-queens/king, blocks, coins, …); `examples/pure_asp/` holds plain-ASP counterparts. They double as manual smoke tests: `funasp examples/<name>.lp`.
