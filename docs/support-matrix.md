# Intensional-function AST support matrix

This matrix records how funasp handles intensional-function occurrences in the
`clingo_funasp.ast` node variants. “Supported” means the occurrence is
translated into a prefixed predicate lookup. “Rejected” means funasp emits a
source-located semantic error instead of allowing clingo to treat the function
as a Herbrand term.

“Supported” covers negated and doubly negated occurrences: negated intensional
literals and comparisons in conditions, aggregate element literals, optimize
elements, and weak-constraint bodies are lifted into auxiliary rules with a
sign-preserving replacement (see the module docstring of
`funasp/ast/_rewritings/negated_literals.py` for the encodings).

## Statements

| AST type | Classification | Intensional-function handling |
|---|---|---|
| `StatementRule` | Supported | Heads and bodies use the position-specific rules below. |
| `StatementOptimize` | Supported | Element tuples and conditions are unnested; negated intensional condition literals and comparisons are lifted. |
| `StatementWeakConstraint` | Supported | Tuple and body occurrences are unnested; negated intensional body literals and comparisons are lifted. |
| `StatementShow` | Partially supported | Functional equations in the condition are supported; rough terms are rejected. |
| `StatementExternal` | Partially supported | Functional equations in the condition are supported; rough terms are rejected. |
| `StatementHeuristic` | Partially supported | Functional equations in the condition are supported; rough terms are rejected. |
| `StatementEdge` | Partially supported | Functional equations in the condition are supported; rough terms are rejected. |
| `StatementProject` | Partially supported | Functional equations in the condition are supported; rough terms are rejected. |
| `StatementShowSignature` | Supported directive | `#showf` is parser-encoded and renamed by the pipeline. |
| `StatementDefined`, `StatementProjectSignature` | Signature-only | No term occurrence to unnest. |
| `StatementComment`, `StatementInclude`, `StatementParts`, `StatementProgram`, `StatementScript`, `StatementShowNothing`, `StatementTheory` | Not applicable | Administrative or theory-definition statements contain no ordinary program-term position handled by FASP. |
| `StatementConst` | Extensional | Constant definitions are processed by clingo; intensional functions are not assignment values in this directive. |

## Rule heads

| AST type | Classification | Handling |
|---|---|---|
| `HeadSimpleLiteral` | Supported | Rough terms are unnested; assignment encodings are preserved. |
| `HeadAggregate` | Supported | Guards, tuples, element literals, and conditions are handled; negated intensional element literals (single and double negation) are lifted. |
| `HeadSetAggregate` | Supported | Guards, element literals, and conditions are handled; negated intensional element literals (single and double negation) are lifted. |
| `HeadDisjunction`, `HeadConditionalLiteral` | Rejected for rough terms | Intensional terms in a disjunctive head raise a semantic error. |
| `HeadTheoryAtom` | Theory-owned | No FASP rewriting is defined inside theory atoms. |

## Rule bodies

| AST type | Classification | Handling |
|---|---|---|
| `BodySimpleLiteral` | Supported | Positive, negative, and double-negative cases are rewritten by their dedicated passes. |
| `BodyConditionalLiteral` | Supported | Generated lookups remain within the condition's local scope; negated intensional condition literals and comparisons are lifted. |
| `BodyAggregate` | Supported | Guards, tuples, and conditions are unnested; negated intensional condition literals and comparisons are lifted. |
| `BodySetAggregate` | Supported | Guards, element literals, and conditions are unnested; negated intensional element literals (single and double negation) are lifted. |
| `BodyTheoryAtom` | Theory-owned | No FASP rewriting is defined inside theory atoms. |

## Maintenance rule

`tests/rewriting/test_support_matrix.py` inventories the statement, head, and
body AST classes exported by the installed `clingo_funasp`. A dependency
upgrade that adds a new variant must update both that test and this matrix.
