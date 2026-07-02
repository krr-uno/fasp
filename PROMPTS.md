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

Make a plan on how to migrate the rest of the application to the new parser. For that recall the correspondence between the two abstract syntax tree of the two parsers. Make questions in case of daubt.

---

# Final summary:

What was done

Verified the upstream fix — clingo-funasp 6.0.0.post11 fixes the parse_files segfault (raw API, wrapper, and error path all work), which unblocked the full migration in one pass.

New pipeline (funasp/rewriting/) — re-implements the semantic rewriting over the parser's F-encoded clingo AST: prefixes (applies --prefix-fun by renaming the hardcoded F/FS), collectors, some_assignments (FS aggregate → choice =1 + #count≥1), aggregates, negated_literals, unnesting + literals, comparisons (f(t)=v → Ff(t,v)), restore (needed after all for the mixed-arity pool case), constraints, orchestrated by integration.rewrite_statements. Validated by a differential harness before cutover: 86/86 exact string matches with the old pipeline across 31 curated programs and all 13 example files, under both F and G prefixes.

Cutover — Control.parse_string/parse_files now use the clingo_funasp parser + new pipeline; the parse-error regex handles file-based locations; the two parse_files tests are live (line/file asserted correctly).

Deleted — tree-sitter parser, funasp/fun_ast/ (nodes + old rewritings), funasp/tools/, funasp/util/tree_sitter.py, tests/syntax_tree/; tree-sitter dropped from pyproject, tree-sitter-fasp from requirements and env files. Reused pieces (RewriteContext, SymbolSignature, collect_variables, term-level unnesting, restore) moved into funasp/rewriting/. test_parser2.py moved to tests/parser/. CLAUDE.md/AGENTS.md rewritten for the new architecture.

Verification — nox -s test, slow_test (all integration/example model expectations unchanged), typecheck (mypy --strict), format all green; 100% coverage maintained. CLI smoke: coloring solves with 6 correct models; --mode=rewrite --prefix-fun=G works.

Two things you should know

1. New upstream bug found: the parser loses the begin-position file of assignment statements (empty string instead of <string>/filename), which produced malformed error locations like :1:1-<string>:1:16. I added a workaround (integration._fix_statement_location, noted in CLAUDE.md) — removable once fixed in clingo-funasp.
2. Behavioral change in messages: error/info reports now show the statement in its parsed form (operation undefined in: Ff(a+1).) rather than the original FASP syntax (f := a+1.), since the parser desugars before the pipeline sees the text. Locations still point at the right source spot. The two affected test expectations were updated and the change is documented in their docstrings. If you want the original syntax back, it would take a FASP re-printer (F-atom → := form) for processing_statement — say the word and I'll plan it.

One unrelated pre-existing issue: nox -s lint fails because pylint isn't installed in this env (lint isn't part of the default or CI sessions).

✻ Cooked for 26m 28s

❯ /plan
  ⎿  Current Plan
     /home/jorge/.claude/plans/we-are-use-a-ancient-creek.md

     Plan: Migrate the funasp rewriting pipeline to the new clingo_funasp parser

     Context

     Step 1 of the migration is done: tests/syntax_tree/tree_sitter/test_parser2.py documents the
     old→new AST mapping, and funasp.util.ast has parse_string/parse_files wrappers over the new
     parser (parse_files is blocked by an upstream segfault in clingo-funasp 6.0.0.post10).

     Exploration verified that the new parser does syntactic desugaring only:
     - a := t :- B. → Fa(t) :- B. (StatementRule/HeadSimpleLiteral, F-prefix + value as last arg)
     - { a := 1 } :- B. → { Fa(1) } :- B. (HeadSetAggregate)
     - f(X) := #agg{…} :- B. → Ff(X) = #agg { tuple: NONE: cond } :- B. (HeadAggregate, left guard
     Ff(X) with Relation.Equal; no value slot — arity stays the original)
     - c(X) := #some{r;g;b} :- B. → FSc(X) = #sum { r: NONE; … } :- B. (FS prefix)
     - #showf p/1. → #show Fp/2. [true] (value is the generic show/hide flag, not a FASP marker)
     - Body occurrences are NOT touched: b :- a = 1. keeps the plain comparison a=1.

     It does not implement FASP semantics: grounding the parsed output directly is wrong (graph
     coloring → UNSAT; b :- a=1. doesn't derive b), and the built-in ast.rewrite_statement
     corrupts FASP-encoded statements (drops Fb(X,a+X) :- c(X). as "operation undefined"). So the
     semantic pipeline survives and must be re-implemented over the F-encoded clingo AST, replacing
     the FASP-node-based pipeline in funasp/fun_ast/rewritings/.

     User decisions:
     1. --prefix-fun is kept via a post-parse renaming pass (F→<prefix>, FS→<prefix>S).
     2. Old code (tree-sitter parser, _nodes.py, obsolete rewritings, their tests) is deleted in
     this migration.
     3. ~~`Controlmigration postponed until upstream fixes theparse_filessegfault~~ — **UPDATE (2026-06-12): fixed in clingo-funasp 6.0.0.post11** (verified: raw + wrapper + error path). The full migration is unblocked: switchControl, un-skip the parse_files` tests,
     delete old code (the former "Phase 2" is now in scope).

     Implementation findings so far:
     - The new pipeline (funasp/rewriting/: prefixes, collectors, some_assignments, aggregates,
     negated_literals, unnesting, comparisons, constraints, integration) is implemented and
     string-matches the old pipeline on the key programs except the mixed-arity pool case
     (f(1) := 2. p(f(a;b,c)).): comparisons get prefixed before clingo unpools, so non-evaluable
     arities end up wrongly prefixed. Fix: keep restore_non_evaluable_functions after the clingo
     rewrite step (it is clingo-only already) — the plan's "obsolete" call was wrong for pools.
     - File-based parse errors (file.lp:1:6-7: error: ...) don't match _PARSING_ERROR_RE in
     funasp/util/ast.py (expects <string>-style) — extend the regex for parse_files error
     locations.

     Remaining work:
     1. Add restore step to funasp/rewriting/integration.py (port restore_non_evaluable_functions
     into the new package).
     2. Tests tests/rewriting/ (per-stage + integration + differential vs old pipeline + end-to-end
     on tests/examples.py EXAMPLES).
     3. Fix _PARSING_ERROR_RE for file errors; un-skip the two parse_files tests in
     test_parser2.py; drop the # pragma: no cover on the parse_files wrapper.
     4. Switch Control.parse_string/parse_files (funasp/control.py) to funasp.util.ast.parse_* +
     funasp.rewriting.rewrite_statements.
     5. Delete old code, moving the reused pieces into funasp/rewriting/ first:
     RewriteContext (trimmed), SymbolSignature, collect_variables,
     unnesting/literals.py transformer (minus the FASP-node register), restore module.
     Delete: funasp/fun_ast/, funasp/util/tree_sitter.py, funasp/tools/print_ast.py,
     tests/syntax_tree/, old tests/restore_anonymous_term_variables.py helper only if unused by
     new tests. Drop tree-sitter from pyproject and tree-sitter-fasp from requirements.txt.
     6. Update CLAUDE.md / AGENTS.md architecture sections.
     7. Full verification: nox -s test, nox -s slow_test (integration tests through the new
     pipeline must keep their expected model strings), typecheck, format.

     Old pipeline → new pipeline correspondence

     Old pipeline (funasp/fun_ast/rewritings/integration.py:130, on custom FASP nodes):

     ┌────────────────────────────────────────────────────────────────────────────────┬────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
     │                                    Old step                                    │                                                                   Fate on the new representation                                                                   │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ tree-sitter parse → AssignmentRule…                                            │ replaced by funasp.util.ast.parse_string (new parser)                                                                                                              │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ rewrite_showf                                                                  │ obsolete — parser already emits #show Fp/(n+1).                                                                                                                    │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ rewrite_some_choices (#some → choice =1 + #count>=1 body)                      │ reimplement: detect FS-prefixed HeadAggregate, produce HeadSetAggregate of F-atoms with right guard = 1 + prepended body #count{…} >= 1                            │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ normalize_assignment_aggregates (f(X):=#agg{…} → f(X):=W + body agg)           │ reimplement: detect F-prefixed HeadAggregate, produce head Ff(X,W) + body BodyAggregate W = #agg{…} (fresh W var; elements lose the NONE literal slot:             │
     │                                                                                │ HeadAggregateElement(tuple, literal=None, condition) → BodyAggregateElement(tuple, condition))                                                                     │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ rewrite_negate_body_literals (not l → #false : l)                              │ survives — already clingo-AST-level; new copy without the AssignmentRule type unions                                                                               │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ collect_evaluable_function_signatures (from AssignmentRule heads)              │ reimplement: collect from prefixed heads — HeadSimpleLiteral/HeadSetAggregate element atoms Pf/n → (f, n−1); HeadAggregate left term Pf/n → (f, n); PSf/n → (f, n) │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ unnest_ast (nested evaluable f(t) → fresh FUN var + f(t)=FUN body comparison)  │ reimplement driver on clingo-only nodes (HeadSimpleLiteral with P-atom, HeadSetAggregate, body literals/aggregates/conditionals, optimize/weak constraints); the   │
     │                                                                                │ term-level logic in unnesting/literals.py is largely reusable (operates on unprefixed terms + evaluable_functions)                                                 │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ to_asp head rewriting (NormalForm2PredicateTransformer._rewrite_head)          │ obsolete — parser already emits prefixed heads                                                                                                                     │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ to_asp body dispatch (LiteralComparison f(t)=v → Pf(t,v) if evaluable, with    │ reimplement as its own step (comparisons.py) — body equalities are unprefixed in the new representation, including the ones emitted by unnesting                   │
     │ pool/candidate-arity handling)                                                 │                                                                                                                                                                    │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ _clingo_rewrite (ast.rewrite_statement + lib.processing_statement error        │ survives as-is (runs after all FASP encoding is plain ASP)                                                                                                         │
     │ plumbing)                                                                      │                                                                                                                                                                    │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ restore_non_evaluable_functions                                                │ obsolete — only evaluable comparisons get prefixed now, nothing to restore                                                                                         │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ functional_constraints (:- Pf(X…,_), 1 < #count{V: Pf(X…,V)}.)                 │ survives — signature-driven, node-type independent; new copy in the new package                                                                                    │
     ├────────────────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
     │ protecting*.py                                                                 │ already dead (test-only); untouched this phase, deleted in phase 2                                                                                                 │
     └────────────────────────────────────────────────────────────────────────────────┴────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

     Reused as-is (imports, no copies): RewriteContext (funasp/fun_ast/_context.py — fields
     lib, prefix_function, ctx, evaluable_functions are node-type independent),
     SymbolSignature (funasp/fun_ast/types.py), collect_variables + fresh-variable generation
     (funasp/fun_ast/collectors.py), ELibrary/parse_string/ParsingException
     (funasp/util/ast.py).

     New step: prefix renaming (--prefix-fun)

     Runs first, only when prefix != "F". Renames exactly the positions the parser generates:
     - HeadSimpleLiteral / HeadSetAggregate element atoms named F… → <prefix>…
     - HeadAggregate left-guard terms F…/FS… → <prefix>…/<prefix>S…
     - StatementShowSignature names F… → <prefix>…

     Body atoms are untouched (the parser never prefixes bodies). Known, accepted limitation of the
     hardcoded encoding: a user-written head atom Foo(1) is indistinguishable from oo := 1 — the
     old parser distinguished these syntactically; the new one cannot. Document this in module
     docstrings.

     Changes (this phase)

     1. New package funasp/rewriting/ (clingo-AST pipeline)

     funasp/rewriting/
       __init__.py        # exports rewrite_statements
       prefixes.py        # renaming pass F→prefix / FS→prefixS
       collectors.py      # collect_evaluable_function_signatures (prefixed heads)
       some_assignments.py# FS HeadAggregate → choice =1 + #count>=1 body
       aggregates.py      # F HeadAggregate → Pf(args,W) head + body aggregate
       negated_literals.py# not l → #false : l   (clingo-only port)
       unnesting.py       # driver over clingo nodes (reuses fun_ast unnesting/literals.py logic)
       comparisons.py     # f(t)=v → Pf(t,v) for evaluable f (pools handled)
       integration.py     # orchestrator rewrite_statements(context, statements)

     integration.py order (mirrors old two-pass structure):
     1. per statement: rename_prefixes (if needed) → rewrite_some → normalize_aggregates →
     rewrite_negated_literals; accumulate context.evaluable_functions (collection must see all
     statements before pass 2, same as old)
     2. per statement: unnest → prefix_comparisons → _clingo_rewrite
     (reuse the old _clingo_rewrite pattern incl. lib.processing_statement so the
     "operation undefined in: …" / "undefined intensional function" error messages keep working)
     3. append functional_constraints(context)

     Output-parity goal: for the same input, the final statement strings should equal the old
     pipeline's output (same Ff(args,v) encoding, same functional constraints). Where exact string
     equality is impractical (fresh-variable numbering, statement ordering), semantic equality (same
     models) is the fallback — see differential tests below.

     2. Tests tests/rewriting/

     Mirror the per-stage tests in tests/syntax_tree/rewriting/, but parse inputs with
     funasp.util.ast.parse_string (new parser):
     - test_collectors.py, test_some_assignments.py, test_aggregates.py,
     test_negated_literals.py, test_unnesting.py, test_comparisons.py, test_prefixes.py
     - test_integration.py — full pipeline, asserting exact rewritten strings (port expectations
     from the old tests/syntax_tree/rewriting/test_integration.py, e.g. f(1) := Y :- g(Y). →
     Ff(1,Y) :- g(Y). + :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.)
     - test_differential.py — both pipelines still exist: exploit it. For the programs in
     tests/examples.py and the old integration-test inputs, assert old-pipeline output ==
     new-pipeline output (string-level where possible, otherwise solve both with
     clingo_funasp.control.Control and compare model sets)
     - test_end_to_end.py — parse_string → new pipeline → Program.add/join → ground → solve on
     the EXAMPLES from tests/examples.py, asserting the expected model strings (same
     expectations as tests/test_control.py, without touching Control)

     During implementation, capture actual parser/pipeline outputs by running probe scripts (as in
     step 1) before hardcoding expected strings.

     3. Untouched this phase

     Control, FaspApp, the tree-sitter parser, funasp/fun_ast/* (old pipeline keeps passing its
     tests), Model/FunctionSymbol.

     Phase 2 (separate, blocked on upstream parse_files fix) — for reference

     1. Switch Control.parse_string/parse_files to funasp.util.ast.parse_string/parse_files +
     funasp.rewriting.rewrite_statements; un-skip the two parse_files tests in test_parser2.py.
     2. Delete: funasp/fun_ast/ (parser, _nodes.py, old rewritings), funasp/util/tree_sitter.py,
     funasp/tools/print_ast.py (or port to the new parser), tests/syntax_tree/,
     tests/restore_anonymous_term_variables.py; move the reused pieces (RewriteContext,
     SymbolSignature, collect_variables) into funasp/rewriting/.
     3. Drop tree-sitter from pyproject.toml and tree-sitter-fasp from requirements.txt.
     4. Fix FunctionSymbol.from_symbol (funasp/symbol.py:24) hardcoded prefix_len=1 if
     multi-char prefixes should work with --prefix-fun.
     5. Update CLAUDE.md/AGENTS.md architecture sections.

     Verification

     1. python -m unittest discover tests/rewriting -v — new pipeline tests pass.
     2. tests/rewriting/test_differential.py — old and new pipelines agree on all example programs.
     3. nox -s test — entire suite (old + new) passes with the enforced 100% coverage on funasp/
     (new package fully covered; old code untouched, keeps its coverage).
     4. nox -s typecheck (mypy --strict) and nox -s format clean.
     5. Sanity: end-to-end test solves graph coloring (examples/coloring.lp program) correctly via
     the new pipeline — the case that is UNSAT without the pipeline.

# Address printing assignents

The parser migration accomplish has a behavioral change.

Behavioral change in messages: error/info reports now show the statement in its parsed form (operation undefined in: Ff(a+1).) rather than the original FASP syntax (f := a+1.), since the parser desugars before the pipeline sees the text. Locations still point at the right source spot. The two affected test expectations were updated and the change is documented in their docstrings. If you want the original syntax back, it would take a FASP re-printer (F-atom → := form) for processing_statement — say the word and I'll plan it.

Create a function called `ast_to_str` that produces a string from an ast node using the old syntax. If the ast note is `Ff(a+1).`, it prints `f := a+1.`. Similar for the prefix `FS`.

Then use it to restore the error messages in `tests/rewriting/test_integration.py`. For example, the error message
```
<string>:1:6-11: info: operation undefined in:
    Ff(a+1).
note: the following operations are undefined:
    a+1"""
```
in lines 785-788 of the file `tests/rewriting/test_integration.py` should be
```
<string>:1:6-11: info: operation undefined in:
    f := a+1.
note: the following operations are undefined:
    a+1""
```
Make a plan for this change.


## What was done

funasp/rewriting/printer.py (new) — ast_to_str(statement) inverts the parser's F-encoding back to FASP syntax:
- Ff(t…,v) heads → f(t…) := v (pooled atoms handled: common value → f(1;2) := 3, impossible differing-values pool falls back to str())
- HeadSetAggregate with F-atoms → { f(t) := v: cond; … } with guards
- Ff(X) = #agg{…} guards → f(X) := #agg{…}; FSf(X) → f(X) := #some{…}
- F-atom element literals in plain head aggregates → #count{ t: f(t) := v: cond }
- #show Ff/(n+1). → #showf f/n.
- everything else passes through as str(statement)

Wired into error reporting — integration._clingo_rewrite now calls processing_statement(ast_to_str(original)), so all "unsafe variables in:", "operation undefined in:" messages show := syntax again.

Tests — tests/rewriting/test_printer.py (round-trip property for every shape + constructed-node pool fallbacks); restored the original expectations in test_integration.py (f := X :- q(Y)., f := a+1.) and test_control.py, and removed the now-obsolete "parsed form" docstring notes.

Verification — nox -s test (100% coverage incl. printer.py), slow_test, typecheck, format all green; CLI smoke confirms f := a+1. in the message.

⚠️ New upstream bug found while probing

The parser silently drops pool alternatives in assignment heads: f(1;2) := 3. parses to only Ff(1,3). — the 2 branch vanishes (plain p(1;2). keeps both). The old tree-sitter pipeline correctly produced Ff(1,3). and Ff(2,3)., so this is a semantic regression that can't be fixed downstream (the information is lost at parse time). Worth fixing in clingo-funasp alongside the begin-file location bug from earlier; the printer test documents the workaround with a comment.


# Addressing upstream bugs

You mention the following bug:
```
New upstream bug found: the parser loses the begin-position file of assignment statements (empty string instead of /filename), which produced malformed error locations like :1:1-:1:16. I added a workaround (integration._fix_statement_location, noted in CLAUDE.md) — removable once fixed in clingo-funasp.
```
Provide a minimal example that showcases the bug.

## Answer:

Here's the verified minimal reproduction (raw clingo_funasp only, no funasp code — ready for an upstream issue):

"""Assignment statements lose the begin-position file (clingo-funasp 6.0.0.post11)."""
from clingo_funasp import ast
from clingo_funasp.core import Library

with Library(logger=lambda t, msg: print("LOG:", msg)) as lib:
    statements = []
    ast.parse_string(lib, "a.\nb := 1.", statements.append)
    for statement in statements[1:]:
        location = statement.location
        print(str(statement), repr(location.begin.file), repr(location.end.file))

    # Consequence: malformed location in error messages.
    statements = []
    ast.parse_string(lib, "f := X :- q(Y).", statements.append)
    try:
        ast.rewrite_statement(ast.RewriteContext(lib), statements[1])
    except RuntimeError:
        pass

Output:

a.      '<string>'  '<string>'      ← plain rule: both files correct
Fb(1).  ''          '<string>'      ← assignment: begin.file is EMPTY

LOG: :1:1-<string>:1:16: error: unsafe variables in:
  Ff(X) :- q(Y).

The bug: any statement containing an assignment gets location.begin.file == "" while end.file is correct ('<string>', or the filename with parse_files). Plain statements in the same parse are fine. So the desugaring step constructs the statement's begin Position without carrying the file over.

The consequence: when clingo prints a location whose begin and end disagree on the file, it emits both halves, producing the malformed :1:1-<string>:1:16: instead of <string>:1:1-16: — visible in any "unsafe variables" / "operation undefined" diagnostic for an assignment rule.

The downstream workaround is _fix_statement_location in funasp/rewriting/integration.py (copies end.file into the begin position when begin.file is empty) — delete it once this is fixed upstream.

While you're filing that: the second upstream bug from earlier is closely related and likely lives in the same desugaring code — pool alternatives are dropped in assignment heads: ast.parse_string(lib, "f(1;2) := 3.", …) yields only Ff(1,3). (the 2 alternative is lost), whereas p(1;2). correctly keeps its pool. That one is information loss at parse time, so funasp can't work around it.

## Revisit

You mention the following bug:
```
New upstream bug found: the parser loses the begin-position file of assignment statements (empty string instead of /filename), which produced malformed error locations like :1:1-:1:16. I added a workaround (integration._fix_statement_location, noted in CLAUDE.md) — removable once fixed in clingo-funasp.
```
Check whether this is fixed upstream. The new version is already installed in the conda environmet `funasp`. Do not install a new package.

## Second bug

You mention a second bug:
```
The parser silently drops pool alternatives in assignment heads: f(1;2) := 3. parses to only Ff(1,3). — the 2 branch vanishes (plain p(1;2). keeps both). The old tree-sitter pipeline correctly produced Ff(1,3). and Ff(2,3)., so this is a semantic regression that can't be fixed downstream (the information is lost at parse time). Worth fixing in clingo-funasp alongside the begin-file location bug from earlier; the printer test documents the workaround with a comment.
```
Check whether this is fixed upstream. The new version is already installed in the conda environmet `funasp`. Do not install a new package. Does anything need to be done about this?

# Reordering the steps in integration

Looking at the function `rewrite_statements` in the file `funasp/rewriting/integration.py`, the first step is calling the function `rename_prefixes`. Move the call of this function just before the call to `prefix_comparisons`. Because of this changes, all functions in between should use `PARSER_PREFIX` instead of `context.prefix_function`. Make a plan to make the necessary changes to these functions.

## Summary

Moved rename_prefixes from the start of pass 1 to immediately before prefix_comparisons in pass 2, so the configurable --prefix-fun prefix is now introduced at exactly one point in the pipeline. Pass-1 desugaring and unnesting now operate on the parser's fixed F/FS encoding.

Changes:
- integration.py — moved the rename_prefixes call into the pass-2 loop (between unnest_statement and prefix_comparisons); updated the module docstring.
- some_assignments.py, aggregates.py, collectors.py — switched detection/rebuild from context.prefix_function to the parser constant PARSER_PREFIX; updated docstrings.
- prefixes.py — deleted the now-unreachable HeadAggregate left-guard rename branch (pass 1 consumes all such heads before the rename runs); updated the docstring's list of renamed positions.
- CLAUDE.md — updated the two numbered pipeline items.

Verification:
- nox -s test — 101 tests pass, 100% coverage held (no new uncovered branches, no pragmas needed; the dead branch was removed rather than excluded).
- nox -s typecheck — mypy --strict clean.
- black/isort — all five changed files unchanged.
- Smoke test — examples/family.lp produces identical models with the default prefix and --prefix-fun=G.

The pipeline output is byte-identical, so the exact-string integration assertions (including the prefix="G" tests) pass unchanged.


# Some assignments with pools

The function `rewrite_some_assignments` in file `funasp/rewriting/some_assignments.py` does not allow more than one pool on the left guard of the assignment. See assert
```python
assert len(left.term.pool) == 1, f"Terms must be unpooled {left.term}"
```
in line 50 of file `funasp/rewriting/some_assignments.py`. Make a plan to allow pools.
- The function needs to retur a list of `ast.Statement` instead of a single `ast.Statement`.
- The list should contain exactly `ast.Statement` if `len(left.term.pool) == 1`
- If `len(left.term.pool) == n`, then it should return `n` rules. Example:

    | input                       | output |
    |-----------------------------|--------|
    | f(a) := #some{X : p(X)}     | #count { 0,Ff(a,X): Ff(a,X): p(X) } = 1 :- #count { X: p(X) } >= 1. |
    | f(a;b) := #some{X : p(X)}   | #count { 0,Ff(a,X): Ff(a,X): p(X) } = 1 :- #count { X: p(X) } >= 1. #count { 0,Ff(b,X): Ff(b,X): p(X) } = 1 :- #count { X: p(X) } >= 1. |
- Add a unittests for for the function `rewrite_some_assignments`.
- Include at least one example of unittest in the plan.

## Summary

rewrite_some_assignments now supports pooled left guards on #some assignments.

funasp/rewriting/some_assignments.py
- Changed the return type from ast.Statement to list[ast.Statement]. Non-#some statements now pass through as a one-element list.
- Removed the assert len(left.term.pool) == 1 restriction.
- Extracted per-entry construction into a helper _rewrite_some_pool_entry(...); the public function maps it over every entry in left.term.pool, yielding one choice statement per pool entry (one for unpooled terms, n for n entries).

funasp/rewriting/integration.py
- The pass-1 loop now iterates over the returned list, sending each derived statement through normalize_assignment_aggregates → rewrite_negate_body_literals → signature collection, keeping the same original for error locations.

Tests
- New tests/rewriting/test_some_assignments.py with three direct unit tests (single pool, multiple pools, non-#some passthrough), using probe-verified output strings.
- Enabled the previously commented-out test_some_assignment_with_pool integration test (f(a;b) := #some{r;g}. → two choice rules + the functionality constraint).

Note: the dev tools live in the funasp conda env (where clingo_funasp is installed), not clingo6 as CLAUDE.md states — I ran nox via PATH="…/envs/funasp/bin:$PATH". Worth flagging in case the documented env is stale.

# Refactor

Create a module `funasp.core` and move the class `funasp.ast.ELibrary` to `funasp.core`. Ajust the rest of the code as necessary.

Rename the class `funasp.core.ELibrary` to `funasp.core.Library`. Ajust the rest of the code as necessary.

Move the following code:
```python
# Errors for strings use an angle-bracketed name (`<string>:1:2-3: error: ...`)
# while errors for files use the plain file name (`file.lp:1:2-3: error: ...`).
_PARSING_ERROR_RE = r"(?:<(?P<bracketed>.*?)>|(?P<file>.*?)):(?P<line>\d+):(?P<col_start>\d+)-(?P<col_end>\d+): error: (?P<msg>.*)"
_PARSING_ERROR_PATTERN = re.compile(_PARSING_ERROR_RE)


def _process_error(
    library: Library, message: tuple[MessageType, str]
) -> SyntacticError:
    """Convert a clingo parsing error message tuple into a SyntacticError."""
    match = _PARSING_ERROR_PATTERN.match(message[1])
    if not match:  # pragma: no cover
        position = Position(library, "<unknown>", 0, 0)
        location = Location(position, position)
        msg = message[1]
    else:
        file = match["bracketed"] if match["bracketed"] is not None else match["file"]
        msg = match["msg"]
        start = Position(library, file, int(match["line"]), int(match["col_start"]))
        end = Position(library, file, int(match["line"]), int(match["col_end"]))
        location = Location(start, end)
    return SyntacticError(
        location,
        msg,
    )


def parse_string(library: core.Library, code: str) -> list[ast.Statement]:
    """
    Parse a string into a list of AST statements.

    Args:
        library (Library): The library to use for parsing.
        code (str): The code string to parse.

    Returns:
        list[ast.Statement]: The list of parsed AST statements.

    Raises:
        Raises ParsingError if parsing fails.
    """
    parsed: list[ast.Statement] = []
    # The error messages are stored to restore them after parsing
    # The library is set to have no error messages during parsing
    # This avoids mixing errors from previous operations with parsing errors
    # This errors will be returned in the ParsingError if parsing fails
    saved_errors = library.error_messages
    library.error_messages = []
    try:
        ast.parse_string(library.library, code, lambda stmt: parsed.append(stmt))
    except RuntimeError as e:
        if str(e) != "parsing failed":  # pragma: no cover
            raise e
        raise ParsingException(
            [_process_error(library.library, error) for error in library.error_messages]
        )
    finally:
        library.error_messages = saved_errors
    return parsed


def parse_files(library: core.Library, files: Sequence[str]) -> list[ast.Statement]:
    """
    Parse the given files into a list of AST statements.

    Args:
        library (Library): The library to use for parsing.
        files (Sequence[str]): The paths of the files to parse.

    Returns:
        list[ast.Statement]: The list of parsed AST statements.

    Raises:
        Raises ParsingError if parsing fails.
    """
    parsed: list[ast.Statement] = []
    # The error messages are stored to restore them after parsing
    # The library is set to have no error messages during parsing
    # This avoids mixing errors from previous operations with parsing errors
    # This errors will be returned in the ParsingError if parsing fails
    saved_errors = library.error_messages
    library.error_messages = []
    try:
        ast.parse_files(library.library, files, lambda stmt: parsed.append(stmt))
    except RuntimeError as e:
        if str(e) != "parsing failed":  # pragma: no cover
            raise e
        raise ParsingException(
            [_process_error(library.library, error) for error in library.error_messages]
        )
    finally:
        library.error_messages = saved_errors
    return parsed


def transform_iterable[T, R](
    library: Library, iterable: Iterable[T], fun: Callable[[Library, T], R | None]
) -> list[T | R] | None:
    """
    Apply a function to each element of an iterable.
    If all elements are transformed to None, return None. Otherwise, return an iterable of the transformed elements, where elements that were transformed to None are replaced by the original element.

    Args:
        iterable (Iterable[T]): The input iterable of elements of type T.
        fun (Callable[[Library, T], R | None]): A function that takes a Library and an element of type T and returns a transformed element of type R or None.

    Returns:
        list[T | R] | None: A list of transformed elements, or None if all elements were transformed to None.
    """
    result: list[T | R] = []
    all_none = True
    for element in iterable:
        new_element = fun(library, element)
        if new_element is not None:
            all_none = False
            result.append(new_element)
        else:
            result.append(element)
    if all_none:
        return None
    return result
```
from `funasp/util/ast.py` to `funasp/ast.py`.  Ajust the rest of the code as necessary.

---

Modify the functions `parse_string` and `parse_files` in `funasp/ast.py` to return list of objects of class `funasp.ast.Statement`. Modify `rewrite_statements` in `funasp/rewriting/integration.py` to have the following signature:
```python
def rewrite_statements(
    context: RewriteContext,
    statements: Iterable[funasp.ast.Statement],
) -> list[funasp.ast.Statement]:
```
Ajust the rest of the code as necessary.

---

Move module `funasp.rewriting.printer` to module `funasp.printer`. Ajust the rest of the code as necessary.

---

Move `PARSER_PREFIX` from `funasp/rewriting/prefixes.py` to `funasp/ast.py`. Move `SOME_MARKER` from `funasp/rewriting/some_assignments.py` to  `funasp/ast.py`. Ajust the rest of the code as necessary.

---

This is the code you produced for the function `rewrite_statements` in `funasp/rewriting/integration.py`
```python
def rewrite_statements(
    context: RewriteContext,
    statements: Iterable[Statement],
) -> list[Statement]:
    """
    Run the pipeline over parsed statements and return transformed statements.

    Each input :class:`~funasp.ast.Statement` keeps its ``original`` and has its
    ``rewritten`` list filled with the clingo statements it expands to. The
    functionality constraints are appended as additional wrapped statements.
    """
    wrappers = list(statements)
    pass1: list[tuple[Statement, ast.Statement]] = []
    for wrapper in wrappers:
        for stmt in rewrite_some_assignments(context, wrapper.original):
            stmt = normalize_assignment_aggregates(context, stmt)
            stmt = rewrite_negated_body_literals(context, stmt)
            context.evaluable_functions |= collect_evaluable_function_signatures(
                context, stmt
            )
            pass1.append((wrapper, stmt))

    for wrapper in wrappers:
        wrapper.rewritten = []
    for wrapper, stmt in pass1:
        stmt = unnest_statement(context, stmt)
        stmt = rename_prefixes(context, stmt)
        stmt = prefix_comparisons(context, stmt)
        wrapper.rewritten.extend(
            restore_non_evaluable_functions(context, rewritten)
            for rewritten in _clingo_rewrite(context, wrapper, stmt)
        )

    for constraint in functional_constraints(context):
        wrappers.append(Statement(context.lib.library, constraint))
    return wrappers
```
The code of this function is difficult to understand. A better solution is the following:
```python
def rewrite_statements(
    context: RewriteContext,
    statements: Iterable[Statement],
) -> list[Statement]:
    """
    Run the pipeline over parsed statements and return transformed statements.

    Each input :class:`~funasp.ast.Statement` keeps its ``original`` and has its
    ``rewritten`` list filled with the clingo statements it expands to. The
    functionality constraints are appended as additional wrapped statements.
    """
    for stmt in statements:
        stmt.rewrite(partial(rewrite_some_assignments, context))
        stmt.rewrite(partial(normalize_assignment_aggregates, context))
        stmt.rewrite(partial(rewrite_negated_body_literals, context))
        for clingo_stmt in stmt.rewritten:
            context.evaluable_functions |= collect_evaluable_function_signatures(
                context, clingo_stmt
            )
    for stmt in statements:
        stmt.rewrite(partial(unnest_statement, context))
        stmt.rewrite(partial(rename_prefixes, context))
        stmt.rewrite(partial(prefix_comparisons, context))
        stmt.rewrite(partial(_clingo_rewrite, context, stmt))
        stmt.rewrite(partial(restore_non_evaluable_functions, context))

    new_statements = list(statements)
    for constraint in functional_constraints(context):
        new_statements.append(Statement(context.lib.library, constraint))
    return new_statements
```
For achieving this clean, the function `rewrite` was added to the class `Statement` in `funasp/ast.py`.
Remember this for future contributions.

---

I refactor files `funasp/util/ast.py` and `funasp/ast/_rewritings/literals.py` and added test in files `tests/util/test_ast.py` and `tests/rewriting/test_unnesting.py`. Update your understanding of the project and look for possible bugs.

---

In `funasp/util/collectors.py` create a function with signature `def collect_predicates(node: AST) -> set[SymbolSignature]:` that returs the set of all predicate symbols in `node`. Examples:
- For rule `a, b(X) :- c(d,X), not e, not f(7).` we get the set `{a/0, b/1, c/2, e/0, f/1}`.
- For the rule `a : b(X) :- c(d,X), e(Y) : f(Y).` we get the set `{a/0, b/1, c/2, e/1, f/1}`.
- For the rule `:- a(X), #count{ Y : b(X,Y)} > 5.` we get the set `{a/1, b/2}`.

---

Create a function with signagure
```python
def rewrite_negated_head_literals(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
```
in `funasp/ast/_rewritings/negated_literals.py` that moves negated literals from the head to the body. Examples:
- `a, not b, not not c :- d.` becomes `a :- d, not not b, not c.`
- `a(X), not b(X), not not c(X) :- d(X,Y).` becomes `a(X) :- d(X,Y), not not b(X), not c(X).`

---

Function ``rewrite_negated_body_literals`` in ``funasp/ast/_rewritings/negated_literals.py`` rewrites body literal ``not l``  into the conditional literal ``#false : l``. We are going to expand this rewriting to negative literals inside conditional literals. Nested conditional literals are not allowed, so we are going to replace every negated literal of the form ``not p(a,X)`` inside a conditional literals with a new literals ``not RD1(X)` and add the rule `RD1(X) :- p(a,C).`
Examples:
- `a :- b(X); c(X,Y) : d(Y), not e(5,f(Y;Y+2)).` becomes `a :- b(X); c(X,Y) : d(Y); not RD1(Y). RD1(Y) :- e(5,f(Y;Y+2)).`
- Example with two rules:
```
a :- b(X); c(X,Y) : d(Y), not e(5,f(Y;Y+2)).
b(2) :- c(X) : d(X), not f(X).
```
becomes
```
a :- b(X); c(X,Y) : d(Y); not RD1(Y).
b(2) :- c(X) : d(X), not p(g(X,Y)).
RD1(Y) :- e(5,f(Y;Y+2)).
RD2(X,Y) :- p(g(X,Y)).
```
The auxiliary predicates `RD1`, `RD2` are created by incrementing a counter and add it to a prefix. Ensure that auxiliary predicate has not been used anywhere else in the program. To do this:
- collect all predicates in the program at the begining of the pipeline in `funasp.ast._rewritings.rewrite_statements`. Store this in a new argument in the `context`.
- Before using a predicate name, ensure that it is not used before by checking the set crated in the previous step.

Create a plan to do this.

---

funasp/util/collectors.py — added collect_variables_ordered(node) -> list[str]: distinct variable names in order of first occurrence.

funasp/ast/_rewritings/context.py — RewriteContext gained:
- predicates init kwarg (set[SymbolSignature]) holding all program predicates,
- auxiliary_statements: list[ast.StatementRule] accumulating the generated rules,
- fresh_predicate_name(prefix="RD"): increments a counter and skips names colliding with predicates.

funasp/ast/_rewritings/negated_literals.py — new rewrite_negated_condition_literals(context, statement): for each BodyConditionalLiteral, every symbolic single-negation condition literal not p(t…) is replaced by not RDi(v…) (the literal's variables, _ excluded) and an aux rule RDi(v…) :- p(t…). is stashed in the context.

funasp/ast/_rewritings/__init__.py — pipeline: pass 0 collects all predicates via collect_predicates; the lifting runs in pass 1; aux rules are appended (wrapped as Statements) after all originals so pass 2 unnests intensional functions inside them.

Key outcome — the previously rejected case now works end-to-end:
f := 1.  :- q : not p(f).   →   :- q: not RD1.   RD1 :- p(FUN); Ff(FUN).
(the rejection error is still raised for negated aggregate-element conditions, which remain out of scope, and stays test-covered).

Both spec examples verified via the real Control API; note clingo's own rewrite step later splits the pool: RD1(Y) :- e(5,f(Y)). / RD1(Y) :- e(5,f(1*Y+2)). Also note your example-2 rule was unsafe as written (Y only in the negated literal), so the integration test binds it with e(Y) in the condition.

Verification: nox -s test → 153 tests, 100% coverage; nox -s slow_test (full suite incl. test_app.py) passes; mypy --strict clean; formatting clean.

---

Instead of appending the auxiliary statements to `context.auxiliary_statements`, make that `rewrite_negated_head_literals` returns a list of statments. That is, the signature becomes
```python
def rewrite_negated_head_literals(
    context: RewriteContext, statement: ast.Statement
) -> list[ast.Statement]:
```

---

Note: your message named rewrite_negated_head_literals, but the function that produces auxiliary statements is rewrite_negated_condition_literals — I applied the change there (the head-literals function creates no aux statements and kept its single-statement signature).

Changes:

- rewrite_negated_condition_literals now returns list[ast.Statement] — the rewritten rule followed by its auxiliary rules. Unchanged inputs return [statement] (identity element). The auxiliary list is threaded through _rewrite_conditional_literal / _lift_condition_literal via partial instead of the context.
- RewriteContext.auxiliary_statements removed (along with the now-unused ast import); predicates and fresh_predicate_name remain.
- rewrite_statements no longer has the append loop — Statement.rewrite handles the list return natively, so aux rules live in the originating statement's rewritten list and flow through pass 2 automatically. (I kept your relocation of the step to after signature collection.)

Behavioral consequence: aux rules now appear right after their originating rule instead of at the end of the program:

a :- b(X); c(X,Y): d(Y), not RD1(Y).
RD1(Y) :- e(5,f(Y)).
RD1(Y) :- e(5,f(1*Y+2)).
b(2) :- c(X): d(X), e(Y), not RD2(X,Y).
RD2(X,Y) :- p(g(X,Y)).

A side benefit: since aux rules are bundled with their origin Statement, error reports during later pipeline stages point at the user's original rule.

Tests updated accordingly (unit tests now assert the returned list; integration test expects the interleaved order). Verified: nox -s test → 153 tests, 100% coverage; mypy --strict clean; formatting clean.

---

The variables do not need to be writen in order of occurrence. Write them in the order produced by `sorted()`. As a ressult, you can use `collect_variables` instead of `collect_variables_ordered`. You can remove the later.

---

Let us plan how to extend this rewriting to `Aggregregates` and `SetAggregates` both in the head and in the body. Apply it to `HeadConditionalLiteral` if you did not do it already. Before executing further elaborating the plan, write a couple of examples for each case and show them to me for aproval.

---

Only LiteralSymbolic with sign == ast.Sign.Single. Comparisons (not f = X) and double negation are untouched (consistent with rewrite_negated_body_literals, which also only handles Sign.Single).

Untouched: not not literals, comparisons, the conditional's main literal, non-rule statements.

---


# Future work

- Good error messages during grounding.
- Consider. Move `RewriteContext` from `funasp.rewriting._context` to `funasp/ast.py`. Move `rewrite_statements` from `funasp/rewriting/integration.py` to `funasp/ast.py`. Ajust the rest of the code as necessary.