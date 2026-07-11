Report: HEAD (fasp/jorge/agg-not) vs branch jorge/agg-not

> Historical snapshot of a branch review; the bugs listed have since been fixed.

jorge/agg-not is a direct ancestor of HEAD, so the diff is exactly the six bug-fix commits on top of it (one of which, A4, was reverted and redone). Net: 14 files, +435/−12. Verification status: nox -s test passes (188 tests, 100% coverage), mypy --strict clean, but black --check fails on three new test files, so CI's format session will fail.

1. Changes made

A4 — validate --prefix-fun (commits 5f3265f, e7a9ede revert, 2439cec redo)
- New _validate_prefix_collisions step in funasp/ast/_rewritings/__init__.py:61, run right after predicate collection: rejects an empty prefix and any prefix that is a prefix of a user predicate name (e.g. --prefix-fun go with predicate good/2), raising a RewritingException with the colliding signatures.
- New --ignore-prefix-collisions flag wired through FaspApp (app.py) and Control (control.py) into RewriteContext.
- RewriteContext._auxiliary_prefix (context.py:42): fresh auxiliary predicates now avoid names starting with the function prefix by falling back from RD to AD (e.g. with --prefix-fun R).
- Model.function_symbols now passes prefix_len=len(self.prefix) to FunctionSymbol.from_symbol, so multi-character prefixes render correctly.

C2 — hide auxiliary predicates from answer sets (b4c1ad4)
- funasp/solve.py: _is_hidden_auxiliary_symbol filters RD*/AD* atoms out of both Model.symbols and Model.function_symbols.

D2 — log normalization honors the configured prefix (29748e7)
- Library gains a prefix_function attribute (default "F", set by Control and RewriteContext); normalize_log_message builds "undefined predicate <prefix>" dynamically instead of hardcoding F.

D1 — double-negated literals over intensional functions (b0d3ae8)
- make_equation (util/ast.py:552) now always emits a positive f(t) = FUN comparison instead of propagating Sign.Double, so unnesting inside not not p(f(a)) binds FUN rather than producing an unsafe-variable error.

Docs and tests
- New bug_fixes.md tracking the status of each item in bug_report.md; docstring step renumbering in _rewritings/__init__.py.
- New tests: end-to-end (hidden auxiliaries, double negation binding/failing), integration (multi-char prefix, collision rejected/ignored, empty prefix), negated-literal auxiliary prefix selection, log-normalization with custom prefix, and a new tests/test_symbol.py.

2. Possible bugs

B1 (confirmed, regression). Empty program crashes. _validate_prefix_collisions reads statements[0].original.location before any emptiness check; Control.parse_string("") now raises a bare IndexError (verified in the funasp env). Guard with if not statements: return or use a synthetic location.

B2 (confirmed). --prefix-fun RD or AD silently swallows all function output. _is_hidden_auxiliary_symbol matches any atom starting with RD/AD — including the renamed function atoms themselves. Verified: f(a) := 1. p(2). with prefix RD prints only p(2), with no error. The collision validator checks user predicates but doesn't reserve the auxiliary prefixes; the cleanest fix is to reject prefixes starting with RD/AD in _validate_prefix_collisions (or to exempt the active function prefix inside _is_hidden_auxiliary_symbol).

B3 (confirmed). CI format gate fails. black --check would reformat tests/rewriting/test_integration.py (the closing paren of test_empty_prefix_rejected is under-indented), tests/rewriting/test_negated_literals.py (missing blank line before if __name__), and tests/test_symbol.py. Since the default CI sessions include format, this blocks. Run nox -s format before merging.

B4 (minor, inconsistent error contract). With --ignore-prefix-collisions and an empty prefix, the empty-prefix rejection is skipped and the failure instead surfaces later as a raw ValueError("could not find an auxiliary predicate prefix") from fresh_predicate_name — not a RewritingException, so the CLI's error formatting won't catch it. An empty prefix can never work (it also makes Model.symbols hide every atom, since everything startswith("")), so it should arguably be rejected unconditionally, ignore flag or not.

B5 (minor, pre-existing but adjacent). normalize_log_message still mangles #some predicates: undefined predicate FSc/1 becomes undefined intensional function Sc/1 — the S some-marker isn't stripped. This predates the diff, but since D2 rewrote exactly this branch it's worth folding in.

3. Code quality / readability / simplicity

- Duplicated constant. _AUXILIARY_PREDICATE_PREFIXES = ("RD", "AD") in context.py and _HIDDEN_AUXILIARY_PREFIXES = ("RD", "AD") in solve.py must stay in sync — if a third auxiliary prefix is ever added in context.py, those atoms leak into model output (bug C2 all over again). Move the tuple to one shared home (e.g. funasp/util/types.py or ast/_core.py next to the other prefix constants) and import it in both places.
- Dead sign plumbing. After the D1 fix, make_equation ignores its sign parameter entirely, yet the value is still threaded through replace_term → TermReplacer.sign → make_equation (util/ast.py:549,569,586,614,625,646). Delete the parameter chain; it now only misleads readers into thinking the equation sign is configurable.
- Three owners of Library.prefix_function. It's set in Library.__init__ (default), Control.__init__, and again in RewriteContext.__init__ (self.lib.prefix_function = prefix_function). Having RewriteContext mutate the shared Library as a side effect is surprising; pick one owner (Control seems natural) or pass the prefix explicitly to normalize_log_message.
- Filter readability in Model.symbols. The predicate grew into a nested three-clause boolean. A single helper — _is_internal_symbol(symbol, prefix) covering both the function prefix and auxiliary prefixes — would make both symbols and function_symbols one-line filters and keep the two hiding rules in one place.
- _prefix_collisions deserves a comment. The not signature.name[:1].isupper() clause is load-bearing (it exempts parser-generated F… atoms, which are the only way a predicate can start uppercase) but reads as arbitrary. One comment line stating the invariant would help.
- Good: the fix pattern is consistent with house style (validation as its own pipeline step, per-concern modules, tests pinning exact rewritten output), and bug_fixes.md cleanly documents the reverted-then-redone A4 decision (dropping the one-uppercase-letter restriction to allow prefixes like __csp_).

The two behavioral bugs (B1, B2) are both in the new A4/C2 code and are cheap to fix — a guard clause and a reserved-prefix check. Happy to apply those fixes if you want.