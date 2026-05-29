# funasp — Agent Instructions

**funasp** extends [clingo 6](https://potassco.org/clingo-preview/python-api/clingo.html) with evaluable (intensional) functions via assignment rules:
```prolog
f(t1) := t2 :- Body.          % deterministic assignment
{ f(t1) := t2 } :- Body.      % choice assignment
f(t1) := #sum{ X : p(X) } :- Body.  % aggregate assignment
```

## Environment

- **Python ≥ 3.13** and **clingo ≥ 6.0.0** are required.
- Recommended setup (conda):
  ```bash
  conda create -n clingo6 python=3.13
  conda activate clingo6
  conda install -c potassco/label/dev-20 -c conda-forge clingo
  pip install -r requirements.txt
  pip install -e .
  ```
- Run the solver: `funasp examples/family.lp`

## Build & Test

All dev tasks run via **nox** (uses the current env, no new venv):

| Command | What it does |
|---|---|
| `nox -s test` | Unit tests + 100% coverage check (excludes slow integration tests) |
| `nox -s ftest` | Fast tests only (no integration tests) |
| `nox -s slow_test` | Full test suite including integration tests |
| `nox -s format` | Format with black + isort + autoflake |
| `nox -s typecheck` | mypy `--strict` on `funasp/` |
| `nox -s lint` | pylint on `funasp/` |

Run a single test file directly:
```bash
coverage run -m unittest tests/syntax_tree/rewriting/test_integration.py -v
```

**100% coverage is enforced** on `funasp/` (tests excluded). Every new code path needs a test.

## Architecture

```
funasp/
  app.py              # CLI entry (FaspApp wraps clingo App interface)
  control.py          # FASP-aware Control: parse → rewrite → solve
  solve.py            # Solve/model utilities
  symbol.py           # Symbol helpers
  syntax_tree/
    _nodes.py         # FASP AST node types (AssignmentAST hierarchy)
    _context.py       # RewriteContext (shared state across rewriting)
    types.py          # SymbolSignature
    collectors.py     # AST collectors
    parsing/
      parser.py       # Tree-sitter-based FASP parser → mixed clingo+FASP AST stream
    rewritings/
      integration.py  # Pipeline orchestrator: rewrite_statements()
      to_asp.py       # Final step: FASP AST → clingo AST rules
      unnesting/      # Unnests complex terms in assignment heads/bodies
      aggregates.py   # Normalizes aggregate assignments
      negated_literals.py
      protecting.py / protecting_operations.py
      restore_anonymous_term_variables.py
      restore_non_evaluable_functions.py
      some_assignments.py   # Choice assignments { f(X) := v }
      showf.py              # #showf directives
  util/
    ast.py            # ELibrary (ctypes clingo wrapper) + AST helpers
```

## Key Conventions

- **FASP AST nodes** subclass `AssignmentAST` (`_nodes.py`) and mirror the clingo AST interface: `to_dict()`, `update(**kwargs)`, `visit(visitor)`.
- **Rewriting pipeline** is driven by `rewrite_statements(ctx, statements)` in `integration.py`. Each step is a separate module under `rewritings/`.
- **Parser** uses `tree_sitter_fasp` to parse FASP-specific syntax, then merges custom FASP nodes with standard `clingo.ast` nodes ordered by source location.
- **Integration tests** (`test_app.py`, `test_app_patch.py`, `test_control.py`) are slow and separated; fast unit tests live under `tests/syntax_tree/rewriting/`.
- Code style: **black + isort + autoflake** (run `nox -s format`).

## Key Files for Understanding Patterns

- [funasp/syntax_tree/_nodes.py](funasp/syntax_tree/_nodes.py) — all custom AST nodes
- [funasp/syntax_tree/rewritings/integration.py](funasp/syntax_tree/rewritings/integration.py) — rewriting pipeline entry point
- [funasp/syntax_tree/rewritings/to_asp.py](funasp/syntax_tree/rewritings/to_asp.py) — final clingo translation
- [funasp/control.py](funasp/control.py) — how parse + rewrite + solve fit together
- [tests/syntax_tree/rewriting/test_integration.py](tests/syntax_tree/rewriting/test_integration.py) — example rewriting tests
- [examples/family.lp](examples/family.lp) — simple FASP example
