# asp2funasp: ASP-to-FUNASP Converter

**Context Summary**: This document provides system architecture and design context for the `asp2funasp` project. Use this as the foundational reference when adding specific requirements or debugging.

## Project Goal

`asp2funasp` is a mini-project that automatically converts **ASP (Answer Set Programming)** code into **FUNASP (Functional ASP)** code. The conversion works by:
1. Analyzing ASP programs to detect predicates that behave like deterministic functions
2. Applying preprocessing transformations to normalize rules into standard forms
3. Identifying and storing functional patterns via pattern matching
4. Rewriting identified functional predicates using FUNASP's assignment rule syntax (`:=`)
5. Converting clingo AST nodes to corresponding FUNASP AST nodes where applicable

## Parent Project: FUNASP

FUNASP extends clingo 6 with **evaluable (intensional) functions** via assignment rules:

```prolog
f(t1) := t2 :- Body.          % deterministic assignment
{ f(t1) := t2 } :- Body.       % choice assignment
f(t1) := #sum{ X : p(X) } :- Body.  % aggregate assignment
```

Key resources:
- **funasp/** (parent directory) — Main FUNASP implementation
- **funasp/fun_ast/_nodes.py** — FASP AST node types (mirrors the clingo AST interface)
- **funasp/fun_ast/rewritings/integration.py** — Rewriting pipeline orchestrator

## Directory Structure

```
asp2funasp/
├── asp2funasp/
│   ├── pattern_finders/           # Detect functional predicates in ASP programs
│   │   ├── aggregate_pattern_finder.py      # Find aggregate-based patterns
│   │   ├── inequality_constraint_finder.py  # Find inequality-based functional predicates
│   │   └── pattern_finder_utils.py          # Shared utilities for pattern matching
│   ├── transformers/
│   │   └── preprocessing/         # Normalize rules before pattern detection
│   │       ├── base.py                      # Base transformer class
│   │       ├── aggregate_head_body_condition_rewrite.py
│   │       ├── choice_rule_guard_normalize_rewrite.py
│   │       ├── negated_comparison_head_to_body_rewrite.py
│   │       ├── notaggregate_constraint_rewrite.py
│   │       ├── constraint_aggregate_guard_normalization.py
│   │       └── __init__.py                  # Pipeline entry: processPipelinetransformers()
│   ├── rewriting/
│   │   ├── functional_predicate_finder.py   # Main orchestrator: finds patterns and generates FPredicate/FRelation
│   │   └── rewrite_into_funasp.py           # Converts identified predicates to FUNASP syntax
│   ├── util/
│   │   ├── types.py               # Data type definitions (FPredicate, CPredicate, FRelation)
│   │   └── util.py                # Utility functions
│   └── __init__.py
├── tests/                         # Test suites organized by component
│   ├── test_pattern_finders/
│   ├── test_transformers/
│   ├── test_rewriting/
│   └── test_util/
├── noxfile.py                     # Build/test automation (local env, no venv)
└── PROMPT.md                      # This file
```

## Core Data Types

Defined in **asp2funasp/util/types.py**:

### FPredicate
```python
FPredicate = namedtuple(
    "FPredicate",
    ["name", "arity", "arguments", "values", "condition"]
)
```
Represents a functional predicate discovered in the program:
- **name**: Predicate name (e.g., "f")
- **arity**: Number of arguments
- **arguments**: Indices of invariant positions (position numbers that don't determine output)
- **values**: Indices of positions that determine the output value(s)
- **condition**: Any conditions needed to make it functional (e.g., body conditions)

### CPredicate
```python
CPredicate = namedtuple("CPredicate", ["name", "arity", "arguments"])
```
Represents constraints or computed predicates.

### FRelation
```python
FRelation = namedtuple("FRelation", ["name", "arity", "arguments", "values"])
```
A functional relation where:
- All positions are either arguments (invariants) or values (outputs)
- No conditions required
- **values** is a tuple of tuples, each representing one value combination
- Derived from `FPredicate` when `condition == []` and fully partitions the predicate arity

**Example**: A predicate `p(1, 2, 3)` with `arguments=[0]` and `values=[[1,2]]` means:
- Position 0 is invariant
- Positions 1 and 2 together determine the output
- This is a function: given position 0, we can determine positions 1 and 2

## Pipeline Architecture

### Stage 1: Preprocessing (transformers/preprocessing/)

Before pattern detection, rules are normalized:
- Split complex aggregates into canonical forms
- Move constraints to body when needed
- Normalize guards and conditions
- Entry point: `processPipelinetransformers(lib, statements)` in transformers/preprocessing/__init__.py

**Why?** Pattern finders work on well-formed rules; preprocessing eliminates AST variations.

### Stage 2: Pattern Detection (pattern_finders/ + functional_predicate_finder.py)

Two pattern detection strategies:

#### 2a. Inequality-based detection (InequalityConstraintFinder)
- Finds functional predicates from inequality constraints like: `:- p(X,Y); p(X,Z); Y != Z.`
- Detects that position 0 (X) determines positions 1 (Y) and 2 (Z)

#### 2b. Aggregate-based detection (AggregatePatternFinder)
- Finds functional predicates from aggregate patterns
- Detects count/sum constraints indicating functionality
- Also finds count constraint patterns

**Coordinator**: FunctionalPredicateFinder (rewriting/functional_predicate_finder.py)
- Calls both finders
- Collects FPredicate results
- Groups and processes into FRelation (via `processFoundPredicates()`)

### Stage 3: AST Rewriting (rewriting/rewrite_into_funasp.py)

**Current state**: Partially implemented

#### Implemented:
- **FunctionalBodyRewriteTransformer** transforms body literals
  - Detects symbolic literals (predicate calls) matching identified FRelations
  - Rewrites them into comparison literals: `f(args) = values`
  - Uses FRelation index for O(1) lookup

**Example**:
```prolog
% ASP: body contains p(1, Y, Z)
p(1, Y, Z)

% With FRelation: name=p, arguments=[0], values=[[1,2]]
% Rewrites to FUNASP literal comparison:
p(1) = (Y, Z)
```

#### Not yet implemented:
- **Head rewriting**: Converting predicate definitions to assignment rules
  - ASP: `p(X, Y) :- ...`
  - FUNASP: `p(X) := Y :- ...`
- **AST node conversion**: Changing clingo AST nodes to FASP AST nodes
  - Body literal comparisons use clingo AST (LiteralComparison)
  - Need to integrate FASP AST nodes for assignment heads
  - Reference: funasp/fun_ast/_nodes.py for FASP node types

## Key Integration Points

### 1. Clingo AST Manipulation
- Uses clingo's `Library` (ctypes wrapper from funasp.util.ast)
- AST visitors follow clingo's interface: `transform()`, `update()`, `visit()`
- Helper utilities:
  - `is_function(term)` — checks if term is a function call
  - `function_arguments_ast(lib, term)` — extracts function args from AST
  - `function_arguments(symbol)` — extracts function args from evaluated symbol

### 2. FRelation Index
- `index_frelations(frelations: List[FRelation])` creates lookup: `SymbolSignature → FRelation`
- SymbolSignature = (name, arity) tuple for fast predicate matching

### 3. FUNASP AST Integration
- Assignment rule nodes: AssignmentAST subclasses from funasp/fun_ast/_nodes.py
- Need to construct FASP nodes during rewriting (currently only using clingo nodes)

## Testing Strategy

- **Unit tests** under `tests/` — fast, focused on single components
- **Pattern finder tests** — verify detection logic on various ASP patterns
- **Transformer tests** — verify preprocessing output
- **Rewriting tests** — verify AST transformation correctness
- Run all tests: `nox -s test` (from asp2funasp root)

## Current State & Next Steps

### Completed ✓
- Pattern detection infrastructure (FPredicate, FRelation identification)
- Preprocessing pipeline (rule normalization)
- Body literal rewriting (ASP predicates → FASP comparisons)
- FRelation indexing and lookup

### In Progress
- **Literal body rewrites** with FRelation info (partially done)

### TODO
- **Head rewriting**: Rewrite rule heads `p(X, Y)` → `p(X) := Y`
- **AST node conversion**: Integrate FASP AST nodes (AssignmentAST) for assignment heads
- **Aggregate rewriting**: Handle aggregate assignments (e.g., `f(X) := #sum{ ... }`)
- **Testing**: Expand test coverage to match FunctionalBodyRewriteTransformer
- **End-to-end**: Pipeline from ASP input file → FUNASP output file

## Code Style & Conventions

- **Format**: black + isort + autoflake — run `nox -s format` before committing
- **Type checking**: mypy --strict on funasp/ — run `nox -s typecheck`
- **Linting**: pylint — run `nox -s lint`
- **AST patterns**: Follow clingo's visitor/transformer pattern for node traversal
- **Naming**:
  - `F*` prefix for functional objects (FPredicate, FRelation)
  - `C*` prefix for constraint/computed objects
  - `*Finder`, `*Transformer` for visitor/transform classes

## Debugging Tips

1. **Pattern detection not working?**
   - Check preprocessing output — rules may not match expected form
   - Verify FPredicate.condition — only empty conditions → FRelation
   - Print FRelation index to ensure predicates are indexed

2. **Rewriting missing predicates?**
   - Check SymbolSignature key construction (name, arity)
   - Verify FRelation is in index before rewrite attempt
   - Print intermediate AST before/after rewrite

3. **AST structure errors?**
   - Check clingo AST node constructors — order and type of arguments matter
   - Use clingo.ast.show() for debugging AST structure
   - Reference funasp/fun_ast/_nodes.py for FASP node constructors

## External Dependencies

- **clingo ≥ 6.0.0** — logic solver and AST framework
- **funasp** — parent project with FASP implementation and utilities
- **tree_sitter_fasp** — optional, used for FASP-specific parsing (not core to asp2funasp detection)

## File Reference

### Entry Points
- **functional_predicate_finder.py** — Main orchestrator; call `FunctionalPredicateFinder.find()`
- **rewrite_into_funasp.py** — Rewriting transformer; instantiate `FunctionalBodyRewriteTransformer`

### Must-Know Files
- **types.py** — Understand FPredicate/FRelation structure thoroughly
- **integration.py** (in rewritings/) — Rewriting pipeline orchestrator pattern
- **funasp/fun_ast/_nodes.py** — Reference for FASP AST nodes

---

**When adding a requirement, describe:**
1. Which stage(s) it affects (preprocessing, detection, rewriting)
2. What AST node types are involved
3. Any new data types or FRelation properties needed
4. Example input/output (ASP → FUNASP transformation)
