We are use a new parser that accepts the same language but uses a different internal representacion.
Instead of using functions `funasp.fun_ast.parsing.parser.parse_string` and `funasp.fun_ast.parsing.parser.parse_files` we are going to use functions `clingo_funasp.parse_string` and `clingo_funasp.parse_files`.

We are going to tart by creating a file `tests/syntax_tree/tree_sitter/test_parser2.py` that contains test analogous to the ones in `tests/syntax_tree/tree_sitter/test_parser.py` but for the new functions. Uses these tests to map between the old and new representation of the abstract syntax tree.

For example, function `funasp.fun_ast.parsing.parser.parse_string` parses `a := 1 :- b11; b12.`  as an `AssignmentRule` rule whose head is an `HeadSimpleAssignment`. Function `clingo_funasp.parse_string` parses `a := 1 :- b11; b12.` as a `StatementRule` whose head is a `HeadSimpleLiteral`. The fact that is an assigment is marked by the fact that the name of the symbol is `Fa` instead of `a`.

---

Summary of what was done:

Changes

tests/syntax_tree/tree_sitter/test_parser2.py (new) — 15 tests mirroring test_parser.py against the new clingo_funasp parser, documenting the old→new AST mapping in the module docstring and per-test docstrings:

┌───────────────────────────────────────┬────────────────────────────────────────────────────────────────────────────────────────────┐
│           Old (tree-sitter)           │                                    New (clingo_funasp)                                     │
├───────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────┤
│ AssignmentRule / HeadSimpleAssignment │ StatementRule / HeadSimpleLiteral with F-prefixed name, value as last arg (a := 1 → Fa(1)) │
├───────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────┤
│ ChoiceAssignment                      │ HeadSetAggregate ({ a := 1 } → { Fa(1) })                                                  │
├───────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────┤
│ HeadAssignmentAggregate               │ HeadAggregate with left guard Fa = #sum {...}, Relation.Equal                              │
├───────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────┤
│ ChoiceSomeAssignment (#some)          │ HeadAggregate with FS prefix (FScolor(X) = #sum {...})                                     │
├───────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────┤
│ ShowFDirective                        │ StatementShowSignature name="Fp", arity+1, value=True                                      │
├───────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────┤
│ Head-aggregate assignments            │ element atoms desugared: f(X) := Y → Ff(X,Y)                                               │
└───────────────────────────────────────┴────────────────────────────────────────────────────────────────────────────────────────────┘

Notable behavioral differences captured in the tests: the missing-dot error now reports line 2 (token position) instead of line 1, head locations differ slightly, and the old "merge" test became a statement-order test since there's a single parser now.

funasp/util/ast.py — added a parse_files wrapper analogous to the existing parse_string (list-returning, ParsingException conversion).

⚠️ Upstream bug found

clingo_funasp.ast.parse_files segfaults on any input in clingo-funasp 6.0.0.post10 — verified with plain ASP, FASP syntax, relative/absolute paths, and stdin (parse_string works fine). Per your choice, the wrapper is kept with # pragma: no cover and its two tests are @unittest.skip-ed with the reason recorded; both are trivial to re-enable once the package is fixed.

---

Make a plan on how to migrate the rest of the application to the new parser. For that recall the correspondence between the two abstract syntax tree of the two parsers.