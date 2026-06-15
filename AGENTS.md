# funasp — Agent Instructions

**funasp** extends [clingo 6](https://potassco.org/clingo-preview/python-api/clingo.html) with intensional functions via assignment rules:
```prolog
f(t1) := t2 :- Body.          % deterministic assignment
{ f(t1) := t2 } :- Body.      % choice assignment
f(t1) := #sum{ X : p(X) } :- Body.  % aggregate assignment
color(X) := #some{r;g;b} :- country(X).
```

## Environment

- **Python ≥ 3.13** and **clingo-funasp ≥ 6.0.0** (clingo 6 fork with the FASP parser) are required.
- Recommended setup (conda):
  ```bash
  conda create -n clingo6 python=3.13
  conda activate clingo6
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
python -m unittest tests.rewriting.test_integration -v
```

**100% coverage is enforced** on `funasp/` (tests excluded). Every new code path needs a test.

## Architecture

The `clingo_funasp` parser desugars FASP syntax at parse time into plain clingo AST with a name-prefix encoding: `f(t) := v` becomes the atom `Ff(t,v)` (prefix + value as last argument), `f(X) := #agg{…}` becomes the head aggregate `Ff(X) = #agg{…}`, `#some` uses the `FS` prefix, `#showf p/n` becomes `#show Fp/(n+1).`. Body occurrences stay untouched. The rewriting pipeline turns this syntactic encoding into semantically correct ASP.

```
funasp/
  app.py              # CLI entry (FaspApp wraps clingo App interface)
  control.py          # FASP-aware Control: parse → rewrite → solve
  solve.py            # Model wrapper: renders Ff(t,v) atoms as f(t)=v
  symbol.py           # Symbol helpers
  rewriting/
    integration.py    # Pipeline orchestrator: rewrite_statements()
    _context.py       # RewriteContext (shared state across rewriting)
    types.py          # SymbolSignature
    prefixes.py       # --prefix-fun: rename parser's F/FS to the configured prefix
    collectors.py     # intensional-function signatures from prefixed heads + collect_variables
    some_assignments.py  # FS aggregate → choice =1 + #count>=1 body
    aggregates.py     # Ff(X) = #agg{…} head → Ff(X,W) head + body aggregate
    negated_literals.py  # not l → #false : l
    unnesting.py      # statement-level driver: nested intensional f(t) → FUN var + comparison
    literals.py       # term-level unnesting logic
    comparisons.py    # intensional f(t)=v → Ff(t,v) (pools handled)
    restore.py        # un-prefix unpooled non-intensional entries after clingo rewrite
    constraints.py    # uniqueness constraints :- Ff(X,_), 1 < #count{V: Ff(X,V)}.
  util/
    ast.py            # ELibrary (log capture/normalization), parse wrappers, AST helpers
```

## Key Conventions

- **Rewriting pipeline** is driven by `rewrite_statements(ctx, statements)` in `rewriting/integration.py`. Each step is a separate module; wire new steps there, not inline.
- **Parsing** goes through `funasp.util.ast.parse_string`/`parse_files` (list-returning wrappers over `clingo_funasp.ast.parse_*` that raise `ParsingException`).
- Transformers use `singledispatchmethod` + `node.transform(lib, fn, …)`; return `None` for "unchanged", a new node otherwise; rebuild with `node.update(lib, **changes)`.
- **Integration tests** (`test_app.py`, `test_app_patch.py`, `test_control.py`) are slow and separated; fast unit tests live under `tests/rewriting/` and `tests/parser/`.
- Code style: **black + isort + autoflake** (run `nox -s format`).

## Key Files for Understanding Patterns

- [funasp/rewriting/integration.py](funasp/rewriting/integration.py) — rewriting pipeline entry point
- [funasp/rewriting/comparisons.py](funasp/rewriting/comparisons.py) — the F-prefix encoding of functional equalities
- [funasp/control.py](funasp/control.py) — how parse + rewrite + solve fit together
- [tests/rewriting/test_integration.py](tests/rewriting/test_integration.py) — example rewriting tests (exact-string expectations)
- [tests/parser/test_parser2.py](tests/parser/test_parser2.py) — documents the parser's FASP encoding
- [examples/family.lp](examples/family.lp) — simple FASP example
