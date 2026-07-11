# funasp — Agent Instructions

**funasp** extends [clingo 6](https://potassco.org/clingo-preview/python-api/clingo.html) with intensional functions via assignment rules:
```prolog
f(t1) := t2 :- Body.                     % deterministic assignment
{ f(t1) := t2 } :- Body.                 % choice assignment
f(t1) := #sum{ X : p(X) } :- Body.       % aggregate assignment
color(X) := #some{r;g;b} :- country(X).  % some choice assignment
```

## Environment

- **Python ≥ 3.13** and **clingo-funasp ≥ 6.0.0.post13** (clingo 6 fork with the FASP parser) are required.
- Recommended setup (conda):
  ```bash
  conda create -n clingo6 python=3.13
  conda activate clingo6
  pip install -r envs/requirements.txt
  pip install -e .
  ```
- Run the solver: `funasp examples/family.lp`

## Build & Test

All dev tasks run via **nox** (uses the current env, no new venv):

| Command | What it does |
|---|---|
| `nox -s test` | Tests + 100% coverage check (excludes only `tests/test_app.py`) |
| `nox -s ftest` | Fast tests (also excludes `test_app_patch.py` and `test_control.py`) |
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

The `clingo_funasp` parser desugars FASP syntax at parse time into plain clingo
AST using a syntactic name-prefix encoding:

| FASP source | Parsed representation |
|---|---|
| `f(t) := v :- B.` | `Ff(t,v) :- B.` — value appended as the last argument |
| `{ a := 1 } :- B.` | `{ Fa(1) } :- B.` (`HeadSetAggregate`) |
| `f(X) := #agg{…} :- B.` | `Ff(X) = #agg{…} :- B.` (`HeadAggregate`, no value slot yet) |
| `c(X) := #some{…} :- B.` | `FSc(X) = #sum{…} :- B.` (`FS` marks `#some`) |
| `#showf p/1.` | `#show Fp/2.` (arity increased by one) |
| body occurrence `f(X)=V` | Left untouched as a comparison |

This encoding has no semantics by itself; grounding it without the funasp
rewrite pipeline produces incorrect programs. User-written function names
cannot begin with uppercase letters, keeping parser-generated `F`/`FS` names
distinguishable.

The execution path is **parse → rewrite → clingo ground/solve → print**:

- `funasp/__main__.py` validates runtime versions and starts the application.
- `funasp/app.py` integrates funasp options and error reporting with clingo.
- `funasp/control.py` parses, rewrites, loads, grounds, and solves programs.
- `funasp/solve.py` hides internal predicates and renders assignments as
  `f(args)=value`.

```
funasp/
  app.py                 # CLI integration and error presentation
  control.py             # FASP-aware Control: parse → rewrite → solve
  core.py                # Library wrapper and FASP-aware log normalization
  solve.py               # Model wrapper: renders Ff(t,v) atoms as f(t)=v
  symbol.py              # FunctionSymbol presentation helper
  ast/
    _core.py             # Statement wrapper, parser prefixes, source rendering
    _parsing.py          # parse_string/parse_files and syntax errors
    _rewritings/
      __init__.py        # rewrite_statements() pipeline orchestrator
      context.py         # RewriteContext and fresh auxiliary predicates
      validation.py      # reject rough functions in unsupported AST positions
      collectors.py      # intensional signatures from parser-prefixed heads
      some_assignments.py  # FS aggregate → choice =1 + #count>=1 body
      aggregates.py      # aggregate assignments → simple head + body aggregate
      negated_literals.py  # head/body/condition negation rewrites
      unnesting.py       # statement-level nested-term driver
      literals.py        # term-level f(t) → FUN + comparison logic
      prefixes.py        # parser F/FS → configured --prefix-fun
      comparisons.py     # intensional f(t)=v → Ff(t,v)
      restore.py         # restore non-intensional pooled alternatives
      constraints.py     # per-function uniqueness constraints
  util/
    ast.py               # generic AST construction and term transformation
    collectors.py        # predicate and variable collectors
    iterables.py         # map_none (None means unchanged)
    types.py             # SymbolSignature and internal prefix constants
```

### Rewrite pipeline order

`rewrite_statements` performs these stages in order:

1. Collect used predicates and `#showf` signatures; validate prefix collisions.
2. Rewrite `#some` and ordinary aggregate assignments, expanding pooled
   targets, then collect intensional-function signatures.
3. Reject rough functions in statement forms without unnesting support, using
   source-located semantic errors.
4. Lift negated condition literals, move negated head literals, rewrite
   single-negated body literals, and lift relevant double-negated literals.
5. Unnest rough terms (`f(t)` → fresh `FUN` plus `f(t)=FUN`), rename parser
   prefixes, and translate functional equalities into prefixed predicates.
6. Run clingo's AST rewriting and restore pooled alternatives whose unpooled
   signatures are not intensional.
7. Append uniqueness constraints and register function predicates for
   FASP-aware log normalization.

## Key Conventions

- **Rewriting pipeline** is driven by `rewrite_statements(ctx, statements)` in `funasp/ast/_rewritings/__init__.py`. Each concern belongs in its own module and is wired into that orchestrator.
- **Parsing** goes through `funasp.ast.parse_string`/`parse_files` (list-returning wrappers over `clingo_funasp.ast.parse_*` that raise `ParsingException`).
- Transformers use `singledispatchmethod` + `node.transform(lib, fn, …)`; return `None` for "unchanged", a new node otherwise; rebuild with `node.update(lib, **changes)`.
- Do not silently pass rough intensional terms through unsupported AST positions. Implement the translation or raise a source-located `SemanticError`; update `docs/support-matrix.md` and its executable inventory test.
- **Integration tests** (`test_app.py`, `test_app_patch.py`, `test_control.py`) are separated; pipeline tests live under `tests/rewriting/` and parser tests in `tests/test_parser.py`.
- Code style: **black + isort + autoflake** (run `nox -s format`).

## Design and Domain Documentation

Read these documents before changing language semantics or the translation
architecture:

- [CONTEXT.md](CONTEXT.md) is the normative vocabulary and semantic overview.
  It defines intensional functions, partiality and undefinedness, assignments,
  rough occurrences, flattening, and uniqueness constraints. Use its preferred
  terms in code, diagnostics, tests, and documentation; in particular, say
  **intensional function**, not the older **evaluable function** terminology.
- [ADR 0001](docs/adr/0001-translate-to-clingo-via-flattening.md) records the
  accepted decision to translate FASP into plain ASP and solve it with clingo,
  rather than implementing a dedicated solver or using an SMT/CSP backend.
  Changes that alter the fundamental translation strategy require a new ADR.
- [ADR 0002](docs/adr/0002-parse-time-prefix-encoding.md) records the accepted
  decision to encode assignments in ordinary clingo AST using parser-generated
  `F`/`FS` prefixes. Changes to the parser–rewriter encoding contract require a
  new ADR and coordinated parser, pipeline, and test updates.
- [docs/support-matrix.md](docs/support-matrix.md) is the current contract for
  intensional-function occurrences in every statement, head, and body AST
  variant. Update it whenever a position becomes supported, rejected, or
  inapplicable. Keep it synchronized with
  `tests/rewriting/test_support_matrix.py`, whose AST inventory intentionally
  fails when `clingo_funasp` introduces an unclassified variant.

ADRs explain why foundational choices were made; do not silently rewrite their
decisions to describe a new architecture. Add a superseding ADR when a decision
changes. `CONTEXT.md` and the support matrix describe the current contract and
must be updated alongside semantic behavior.

## Key Files for Understanding Patterns

- [funasp/ast/_rewritings/__init__.py](funasp/ast/_rewritings/__init__.py) — rewriting pipeline entry point
- [funasp/ast/_rewritings/comparisons.py](funasp/ast/_rewritings/comparisons.py) — the F-prefix encoding of functional equalities
- [docs/support-matrix.md](docs/support-matrix.md) — supported, rejected, and inapplicable AST positions
- [funasp/control.py](funasp/control.py) — how parse + rewrite + solve fit together
- [tests/rewriting/test_integration.py](tests/rewriting/test_integration.py) — example rewriting tests (exact-string expectations)
- [tests/rewriting/test_support_matrix.py](tests/rewriting/test_support_matrix.py) — executable AST inventory and leakage checks
- [tests/test_parser.py](tests/test_parser.py) — documents the parser's FASP encoding
- [examples/family.lp](examples/family.lp) — simple FASP example
