# Parser Migration: Custom FASP Nodes to Prefixed Clingo AST

This note explains the parser change in `funasp`: the project moved from a
tree-sitter based parser that produced custom FASP node classes in `_nodes.py`
to the `clingo_funasp` parser, which encodes FASP syntax directly as ordinary
clingo AST with prefixed predicate names.

The short version is:

- Old parser: preserve FASP syntax in custom Python AST nodes such as
  `AssignmentRule`, `HeadSimpleAssignment`, and `ChoiceSomeAssignment`, then
  later translate those nodes to clingo AST.
- New parser: desugar FASP syntax during parsing into standard
  `clingo_funasp.ast` nodes, using `F` and `FS` prefixes to mark intensional
  functions, then run the semantic rewrite pipeline over those prefixed clingo
  nodes.

The new representation is deliberately syntactic. The parser does not implement
the semantics of intensional functions. The rewrite pipeline still does the
semantic work: `#some` choice behavior, aggregate assignments, unnesting,
functional equality rewriting, and uniqueness constraints.

## Project Context

`funasp` is a conservative extension of clingo with intensional functions.
Users write assignment rules:

```prolog
f(t1,t2) := v :- body(t1,t2).
{ f(t) := v } :- body(t).
f(t) := #sum{ X : p(X) }.
color(X) := #some{r;g;b} :- country(X).
```

Internally, funasp translates those constructs to plain ASP and solves with
clingo. An intensional function `f/n` becomes a predicate with one extra value
argument, for example `Ff/(n+1)`. A uniqueness constraint is appended so that a
function cannot take two different values for the same argument tuple.

## The Old Parser

The old parser lived under the historical `funasp.fun_ast` package. In earlier
commits it also appeared under `funasp.ast`; the important old files were:

- `funasp/fun_ast/parsing/parser.py`
- `funasp/fun_ast/_nodes.py`
- `funasp/fun_ast/rewritings/integration.py`
- `tests/syntax_tree/tree_sitter/test_parser.py`

It used `tree_sitter_fasp` to find FASP-specific syntax that clingo could not
parse. It then parsed those pieces into custom node classes and delegated the
plain ASP pieces back to clingo.

The old parse flow was:

1. Parse the complete source with tree-sitter.
2. Find custom nodes such as `assignment_rule`, `showf`, and `showf_signature`.
3. Convert those tree-sitter nodes into custom FASP AST objects from `_nodes.py`.
4. Blank out the bytes occupied by custom FASP statements.
5. Parse the remaining plain ASP text with clingo.
6. Merge the custom FASP statements and clingo statements by source location.

That meant one parsed program was a mixed stream:

```python
AssignmentRule | ShowFDirective | clingo.ast.Statement
```

For example:

```prolog
a := 1 :- b11; b12.
p(X) :- q(X).
```

became approximately:

```text
AssignmentRule(
  head=HeadSimpleAssignment(assigned_function=a, value=1),
  body=[b11, b12],
)
StatementRule(head=p(X), body=[q(X)])
```

The assignment was still represented as an assignment. It was not yet encoded as
`Fa(1)`.

## What `_nodes.py` Did

`_nodes.py` defined the custom FASP AST layer. These classes imitated enough of
the clingo AST interface to let the rest of the code traverse and transform both
clingo nodes and FASP nodes.

The base class was `AssignmentAST`. It provided:

- `to_dict()`: subclass-specific serializable fields.
- `update(...)`: create a copy with changed fields, similar to clingo AST.
- `visit(...)`: traverse child nodes.
- `transform(...)`: recursively transform child nodes.

The main custom node classes were:

| Old node | Purpose | Example syntax |
|---|---|---|
| `AssignmentRule` | Full rule whose head contains a FASP assignment | `f(X) := Y :- p(X,Y).` |
| `HeadSimpleAssignment` | Simple assignment head | `f(X) := Y` |
| `HeadAssignmentAggregate` | Assignment whose value is an aggregate | `f(X) := #sum{Y : p(X,Y)}` |
| `ChoiceSomeAssignment` | Special `#some` assignment | `color(X) := #some{r;g;b}` |
| `ChoiceAssignment` | Choice head containing assignment elements | `{ f(X) := Y : p(X,Y) }` |
| `AssignmentAggregateElement` | Assignment element inside a choice | `f(X) := Y : p(X,Y)` |
| `HeadAggregateAssignment` | Head aggregate that may contain assignment elements | `#count{X : f(X) := Y}` |
| `HeadAggregateAssignmentElement` | Assignment element inside a head aggregate | `X : f(X) := Y : p(X,Y)` |
| `ShowFDirective` | FASP show directive | `#showf f/1.` |

The old type aliases made the mixed tree explicit:

```python
FASP_Statement = ast.Statement | AssignmentRule
FASP_AST = util_ast.AST | AssignmentAST
```

This gave the old pipeline a very explicit representation, but it also meant
the project had to maintain a parallel AST system. Every traversal, collector,
printer, and rewrite step had to know about both clingo AST nodes and the custom
assignment nodes.

## Old Pipeline

The old pipeline in `funasp/fun_ast/rewritings/integration.py` worked in two
phases.

First it rewrote FASP-level nodes while assignments were still custom nodes:

1. `rewrite_showf`
   converted `ShowFDirective` to clingo `#show Ff/(n+1)`.
2. `rewrite_some_choices`
   converted `ChoiceSomeAssignment` into a choice assignment plus a non-empty
   candidate check.
3. `normalize_assignment_aggregates`
   converted aggregate assignment heads into simple assignment heads with a body
   aggregate.
4. `rewrite_negate_body_literals`
   normalized negated literals.
5. `collect_evaluable_function_signatures`
   collected intensional function signatures from `AssignmentRule` heads.

Then it converted from FASP nodes into plain clingo AST:

1. `unnest_ast`
   replaced nested intensional function terms with fresh variables and equality
   comparisons.
2. `to_asp`
   converted assignment heads into prefixed predicate atoms:
   `f(X) := Y` became `Ff(X,Y)`.
3. `ast.rewrite_statement`
   let clingo do its own rewriting.
4. `restore_non_evaluable_functions`
   restored prefixed atoms that were introduced for pooled, non-intensional
   cases.
5. `functional_constraints`
   appended uniqueness constraints.

The old `to_asp` step was where the `F` predicate encoding was introduced. In
the new design, much of that syntactic head encoding is done by the parser
itself.

## The New Parser

The new parser comes from `clingo_funasp`. The public wrappers are:

- `funasp.ast.parse_string`
- `funasp.ast.parse_files`

Those wrappers call:

- `clingo_funasp.ast.parse_string`
- `clingo_funasp.ast.parse_files`

and return `list[funasp.ast.Statement]`. The wrapper class keeps both:

- `original`: the parsed clingo AST statement.
- `rewritten`: the list of clingo AST statements produced by the pipeline.

The parser no longer returns custom assignment nodes. It returns ordinary
`clingo_funasp.ast.Statement` objects. FASP syntax is marked by a name-prefix
encoding:

| FASP source | Parser output |
|---|---|
| `a := 1.` | `Fa(1).` |
| `f(X) := Y :- p(X,Y).` | `Ff(X,Y) :- p(X,Y).` |
| `{ a := 1 } :- b.` | `{ Fa(1) } :- b.` |
| `f(X) := #sum{Y : p(X,Y)}.` | `Ff(X) = #sum { Y: NONE: p(X,Y) }.` |
| `color(X) := #some{r;g;b}.` | `FScolor(X) = #sum { r: NONE; g: NONE; b: NONE }.` |
| `#showf f/1.` | `#show Ff/2. [true]` |
| `p :- f(X) = Y.` | unchanged body comparison: `p :- f(X) = Y.` |

The value of a simple assignment is appended as the last argument:

```prolog
f(a,b) := c.
```

parses as:

```prolog
Ff(a,b,c).
```

For aggregate assignments, the value slot is not present yet because the value
is the result of the aggregate:

```prolog
f(X) := #count{Y : edge(X,Y)}.
```

parses as:

```prolog
Ff(X) = #count { Y: NONE: edge(X,Y) }.
```

The rewrite pipeline later introduces a fresh result variable:

```prolog
Ff(X,W) :- W = #count { Y: edge(X,Y) }.
```

The parser uses `FS` for `#some` assignments. This is a marker, not the final
function prefix. The `S` says "this assignment came from `#some` and needs
special semantics."

## Why Prefixes Are Safe

The parser's hardcoded prefix is:

```python
PARSER_PREFIX = "F"
SOME_MARKER = "S"
PARSER_SOME_PREFIX = "FS"
```

This is safe because user-written function and predicate names cannot start
with an uppercase letter in ASP syntax. Uppercase identifiers are variables.
So a user cannot write a normal function named `Ff` that collides with the
parser-generated encoding.

The runtime prefix can still be configured with `--prefix-fun`. The parser
always emits `F` and `FS`, then `rename_prefixes` changes parser-generated names
to the configured prefix at the correct point in the pipeline.

## New Pipeline

The current pipeline is exported from `funasp.ast` and implemented in:

```text
funasp/ast/_rewritings/
```

The orchestrator is `rewrite_statements(context, statements)` in
`funasp/ast/_rewritings/__init__.py`.

The current flow is:

1. `rewrite_some_assignments`
   reads parser-generated `FSf(...) = #sum {...}` heads and rewrites them into
   choice heads with an exactly-one guard and a non-empty candidate body check.
2. `rewrite_assignment_aggregates`
   reads parser-generated `Ff(...) = #agg {...}` heads and rewrites them into
   `Ff(...,W)` plus a body aggregate defining `W`.
3. `rewrite_negated_body_literals`
   normalizes negated literals for clingo rewriting.
4. `collect_intensional_function_signatures`
   collects intensional signatures from prefixed head atoms after `#some` and
   aggregate assignments have been normalized.
5. `unnest_statement`
   replaces nested intensional function terms with fresh `FUN` variables and
   generated equality comparisons.
6. `rename_prefixes`
   renames parser-generated `F` atoms to the configured prefix if needed.
7. `prefix_comparisons`
   rewrites body comparisons such as `f(X) = Y` into prefixed predicate literals
   such as `Ff(X,Y)` when `f/1` is intensional.
8. `clingo_funasp.ast.rewrite_statement`
   runs clingo's own AST rewriting.
9. `restore_non_intensional_functions`
   fixes pooled cases where a prefixed literal was created for a candidate arity
   that is not actually intensional.
10. `functional_constraints`
    appends one uniqueness constraint for each collected intensional function.

The important design shift is that there is no longer a `to_asp` step that
converts custom `AssignmentRule` nodes into clingo `StatementRule` nodes. The
parser already gives the pipeline clingo `StatementRule` nodes.

## Old-to-New Node Mapping

The migration can be understood as this mapping:

| Old custom representation | New prefixed clingo representation |
|---|---|
| `AssignmentRule(head=HeadSimpleAssignment(...))` | `StatementRule(head=HeadSimpleLiteral(Ff(...,value)))` |
| `ChoiceAssignment` | `HeadSetAggregate` containing `Ff(...,value)` literals |
| `AssignmentAggregateElement` | `SetAggregateElement` whose literal may be `Ff(...,value)` |
| `HeadAssignmentAggregate` | `HeadAggregate` with left guard `Ff(args) = #agg {...}` |
| `ChoiceSomeAssignment` | `HeadAggregate` with left guard `FSf(args) = #sum {...}` |
| `HeadAggregateAssignment` | `HeadAggregate`; assignment elements become `Ff(...,value)` literals |
| `HeadAggregateAssignmentElement` | `HeadAggregateElement` with a prefixed assignment literal |
| `ShowFDirective` | `StatementShowSignature(name="Ff", arity=n+1, value=True)` |
| Plain clingo statements | Plain clingo statements, unchanged |

The body is the main exception: body occurrences of intensional functions are
not prefixed by the parser. This source:

```prolog
ok :- f(X) = Y.
```

still parses as a normal comparison. `prefix_comparisons` handles it later, but
only after the pipeline has collected that `f/1` is intensional.

## Example End-to-End

Input:

```prolog
value(a) := 1.
double(X) := value(X) + value(X) :- item(X).
ok(X) :- double(X) = 2.
#showf double/1.
```

Parser output is already prefixed in the heads and show directive:

```prolog
Fvalue(a,1).
Fdouble(X,value(X)+value(X)) :- item(X).
ok(X) :- double(X) = 2.
#show Fdouble/2. [true]
```

The pipeline then:

1. collects `value/1` and `double/1` from the prefixed heads,
2. unnests nested `value(X)` occurrences,
3. rewrites generated and user-written equalities into `Fvalue(...)` and
   `Fdouble(...)`,
4. runs clingo rewriting,
5. appends uniqueness constraints for `value/1` and `double/1`.

The final solved program is plain ASP over predicates such as `Fvalue/2` and
`Fdouble/2`, while models are displayed back as `value(a)=1` and
`double(X)=...`.

## Error Reporting and Printing

One downside of parse-time desugaring is that the pipeline sees `Ff(a+1).`,
not the original `f := a+1.`. To keep diagnostics user-facing, `Statement.__str__`
uses `_ast_to_str` from `funasp/ast/_core.py` to invert parser-generated shapes
back into FASP syntax where possible.

For example, clingo's operation-undefined message can report:

```text
operation undefined in:
  f := a+1.
```

instead of:

```text
operation undefined in:
  Ff(a+1).
```

This printer only handles the shapes the parser generates. Everything else
falls back to `str(statement)`.

## Files After the Migration

The current parser/rewrite architecture is centered on:

- `funasp/ast/_parsing.py`
  wraps `clingo_funasp.ast.parse_string` and `parse_files`, converts parser
  errors into `ParsingException`, and returns `Statement` wrappers.
- `funasp/ast/_core.py`
  defines `Statement`, parser prefix constants, `transform_iterable`, and the
  FASP-facing `_ast_to_str` printer.
- `funasp/ast/_rewritings/__init__.py`
  orchestrates the rewrite pipeline.
- `funasp/ast/_rewritings/*.py`
  contains focused rewrite passes for `#some`, aggregate assignments,
  comparison prefixing, unnesting, restoring, and constraints.
- `funasp/control.py`
  calls `parse_string` or `parse_files`, rewrites the returned wrappers, joins
  the resulting clingo program, then solves with `clingo_funasp.control.Control`.
- `tests/test_parser.py`
  documents the parser's prefixed internal representation.
- `tests/rewriting/`
  verifies the semantic rewrite pipeline over the new representation.

The old tree-sitter parser, custom `_nodes.py`, and old `funasp/fun_ast`
rewrite pipeline were removed from the live source. Historical references remain
in Git history and in planning notes.

## Practical Consequences for Future Work

When adding parser or rewrite behavior, assume the input to the funasp rewrite
pipeline is ordinary `clingo_funasp.ast`, not custom FASP nodes.

Useful rules of thumb:

- Detect assignment heads by looking for parser-prefixed names in head
  positions, not by checking for `AssignmentRule`.
- Before `rename_prefixes`, use `PARSER_PREFIX` (`F`) and `PARSER_SOME_PREFIX`
  (`FS`), because those are what the parser emits.
- After `rename_prefixes`, use `context.prefix_function`.
- Do not expect body occurrences to be prefixed by the parser.
- Collect intensional function signatures after `#some` and aggregate
  assignments are normalized.
- Keep new semantic behavior in a focused module under
  `funasp/ast/_rewritings/` and wire it through `rewrite_statements`.
- Add tests for every new code path; the project enforces 100 percent line
  coverage on `funasp/`.

The migration reduced the amount of AST infrastructure funasp owns. Instead of
maintaining a parallel `_nodes.py` hierarchy, funasp now shares clingo's AST as
much as possible and reserves its own code for the semantics that clingo does
not know about.
