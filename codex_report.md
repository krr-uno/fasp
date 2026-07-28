Reviewed the clean main...HEAD branch diff. Three findings, ranked by severity:

  1. High — double-negated comparisons lose stable-model semantics.
     funasp/ast/_rewritings/negated_literals.py:234 only lifts singly negated comparisons. A rough double-negated comparison falls through to unnesting and becomes positive recursion.

     f := 1 :- not not f+0 = 1.
     #showf f/0.

     Currently rewrites essentially to:

     Ff(1) :- Ff(FUN), FUN=1.

     This produces only the empty model; preserving not not RD1 should also permit f=1. It contradicts the sign-preserving support claimed in docs/support-matrix.md:9.

  2. High — auxiliary predicate names are reused across incremental loads.
     funasp/control.py:70 constructs a fresh RewriteContext for every parse_string() or parse_files() call. Consequently, funasp/ast/_rewritings/context.py:38 resets the auxiliary counter
     and forgets predicates from earlier loads. Multiple batches can therefore define the same RD1, merging unrelated auxiliary rules and changing models. The allocator and used-predicate
     state should persist for the lifetime of Control.

  3. Medium — positive outer guards requested by the specification are omitted.
     funasp/ast/_rewritings/negated_literals.py:470 lifts double-negated symbolic literals without passing positive rule-body guards. This conflicts with PROMPTS.md:843, which requires
     p(X)—but not not r(X)—in the generated auxiliary rule. Existing expectations in tests/integration/test_negated_literals.py:28 codify the conflicting behavior.

  Suggested fix for the worst issue: generalize comparison lifting to Sign.Single and Sign.Double, preserve literal.sign on the replacement, and pass positive simple guards when processing
  top-level double negation. Add an end-to-end test requiring both the empty and f=1 models for the reproduction above. No pipeline-order or ADR change is needed.

  Validation: nox -s test passes all 264 tests with 100% coverage. No additional baseline code smells were validated.

  Summary: 3 findings; the worst is double-negated comparisons becoming positive recursion and changing stable models.

  ------

  High — multiple aggregate siblings cannot safely bind globals for lifted conditions.

    In funasp/ast/_rewritings/negated_literals.py:391, nested aggregate conditions receive only positive simple-body guards. Another positive aggregate that binds a global variable is
    excluded:

    f(1) := 2.
    p(1).
    :~ X = #count{Z:p(Z)},
       0 < #count{Y:p(Y), not f(Y)+X > 3}. [1@0,X]

    The generated auxiliary omits the first aggregate:

    RD1(X,Y) :- p(Y); f(Y)+X>3.

    Consequently, X is unsafe and rewriting fails. This behavior already existed in 7d681da, so it is not a regression introduced by the new patch.