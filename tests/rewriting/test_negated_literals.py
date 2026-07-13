"""
Unit tests for the head- and condition-literal rewritings in
``funasp.ast._rewritings.negated_literals``.
"""

import unittest

from funasp.ast import RewriteContext, parse_string
from funasp.ast._rewritings.negated_literals import (
    rewrite_double_negated_body_literals,
    rewrite_negated_condition_literals,
    rewrite_negated_head_literals,
)
from funasp.core import Library
from funasp.util.types import SymbolSignature


class TestRewriteNegatedHeadLiterals(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()
        self.context = RewriteContext(self.lib)

    def _rewrite(self, program: str) -> str:
        """Rewrite a single rule and return its string form."""
        statement = parse_string(self.lib, program)[1].original
        return str(rewrite_negated_head_literals(self.context, statement))

    def _canonical(self, program: str) -> str:
        """Return the string form of a parsed rule (printer canonicalization)."""
        return str(parse_string(self.lib, program)[1].original)

    def test_moves_negated_head_literals(self):
        """Negated head disjuncts move to the body with complemented sign."""
        self.assertEqual(
            self._rewrite("a, not b, not not c :- d."),
            self._canonical("a :- d, not not b, not c."),
        )

    def test_moves_negated_head_literals_with_variables(self):
        """The rewriting preserves the arguments of the moved literals."""
        self.assertEqual(
            self._rewrite("a(X), not b(X), not not c(X) :- d(X,Y)."),
            self._canonical("a(X) :- d(X,Y), not not b(X), not c(X)."),
        )

    def test_moves_single_negated_head_literal(self):
        """A doubly negated simple head becomes a constraint."""
        self.assertEqual(
            self._rewrite("not not c :- not b."),
            self._canonical(":- not b, not c."),
        )

    def test_moves_single_negated_head_literal_single_sign(self):
        """A singly negated simple head becomes a constraint."""
        self.assertEqual(
            self._rewrite("not a :- d."),
            self._canonical(":- d, not not a."),
        )

    def test_non_rule_statement_unchanged(self):
        """A non-rule statement is returned unchanged."""
        statement = parse_string(self.lib, "a :- d.")[0].original
        self.assertIs(rewrite_negated_head_literals(self.context, statement), statement)

    def test_positive_simple_head_unchanged(self):
        """A rule with a non-negated simple head is returned unchanged."""
        statement = parse_string(self.lib, "a :- d.")[1].original
        self.assertIs(rewrite_negated_head_literals(self.context, statement), statement)

    def test_aggregate_head_unchanged(self):
        """A rule whose head is neither simple nor a disjunction is unchanged."""
        statement = parse_string(self.lib, "{ a } :- d.")[1].original
        self.assertIs(rewrite_negated_head_literals(self.context, statement), statement)

    def test_positive_disjunction_unchanged(self):
        """A disjunctive head without negated literals is returned unchanged."""
        statement = parse_string(self.lib, "a, b :- d.")[1].original
        self.assertIs(rewrite_negated_head_literals(self.context, statement), statement)


class TestRewriteNegatedConditionLiterals(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()
        self.context = RewriteContext(self.lib)

    def _rewrite(self, program: str) -> list[str]:
        """Rewrite a single rule and return the resulting statement strings."""
        statement = parse_string(self.lib, program)[1].original
        return [
            str(result)
            for result in rewrite_negated_condition_literals(self.context, statement)
        ]

    def _assert_unchanged(self, statement):
        """Assert the statement is returned unchanged as a singleton list."""
        result = rewrite_negated_condition_literals(self.context, statement)
        self.assertEqual(len(result), 1)
        self.assertIs(result[0], statement)

    def test_lifts_negated_condition_literal(self):
        """A negated condition literal is replaced by an auxiliary call."""
        self.assertEqual(
            self._rewrite("a :- b(X); c(X,Y) : d(Y), not e(5,f(Y;Y+2))."),
            [
                "a :- b(X); c(X,Y): d(Y), not RD1(Y).",
                "RD1(Y) :- e(5,f(Y;Y+2)).",
            ],
        )

    def test_lifts_literal_with_two_variables_in_argument(self):
        """The auxiliary call carries the distinct variables of the literal."""
        self.assertEqual(
            self._rewrite("b(2) :- c(X) : d(X), not p(g(X,Y))."),
            [
                "b(2) :- c(X): d(X), not RD1(X,Y).",
                "RD1(X,Y) :- p(g(X,Y)).",
            ],
        )

    def test_counter_increments_across_rules(self):
        """Each lifted literal gets the next auxiliary predicate name."""
        self.assertEqual(
            self._rewrite("a :- b(X) : c(X), not d(X)."),
            ["a :- b(X): c(X), not RD1(X).", "RD1(X) :- d(X)."],
        )
        self.assertEqual(
            self._rewrite("e :- b(X) : c(X), not f(X)."),
            ["e :- b(X): c(X), not RD2(X).", "RD2(X) :- f(X)."],
        )

    def test_skips_used_predicate_names(self):
        """Names already used in the program are skipped by the generator."""
        self.context.predicates.add(SymbolSignature("RD1", 3))
        self.assertEqual(
            self._rewrite("a :- b(X) : c(X), not d(X)."),
            ["a :- b(X): c(X), not RD2(X).", "RD2(X) :- d(X)."],
        )

    def test_zero_variable_literal(self):
        """A negated literal without variables yields a 0-ary auxiliary."""
        self.assertEqual(
            self._rewrite("q :- r(X) : s(X), not t(5)."),
            ["q :- r(X): s(X), not RD1.", "RD1 :- t(5)."],
        )

    def test_anonymous_variables_are_projected(self):
        """Anonymous variables do not become auxiliary arguments."""
        self.assertEqual(
            self._rewrite("q :- r(X) : s(X), not t(X,_)."),
            ["q :- r(X): s(X), not RD1(X).", "RD1(X) :- t(X,_)."],
        )

    def test_lifts_head_conditional_literal(self):
        """A negated literal in a conditional disjunct condition is lifted."""
        self.assertEqual(
            self._rewrite("a(X) : b(X), not c(X) :- d(X)."),
            ["a(X): b(X), not RD1(X) :- d(X).", "RD1(X) :- c(X)."],
        )

    def test_lifts_head_conditional_literal_in_disjunction(self):
        """Bare disjuncts stay while conditional disjunct conditions are lifted."""
        self.assertEqual(
            self._rewrite("p; q(Y) : r(Y,Z), not s(Y,Z) :- t(Y)."),
            [
                "p; q(Y): r(Y,Z), not RD1(Y,Z) :- t(Y).",
                "RD1(Y,Z) :- s(Y,Z).",
            ],
        )

    def test_lifts_choice_element_condition(self):
        """A negated literal in a choice element condition is lifted."""
        self.assertEqual(
            self._rewrite("{ p(X) : q(X), not r(X) } :- s(X)."),
            ["{ p(X): q(X), not RD1(X) } :- s(X).", "RD1(X) :- r(X)."],
        )

    def test_lifts_choice_with_multiple_elements(self):
        """Each choice element condition gets its own auxiliary predicate."""
        self.assertEqual(
            self._rewrite("1 { a : not b; c(X) : d(X), not e(f(X)) } :- g(X)."),
            [
                "1 <= { a: not RD1; c(X): d(X), not RD2(X) } :- g(X).",
                "RD1 :- b.",
                "RD2(X) :- e(f(X)).",
            ],
        )

    def test_lifts_head_aggregate_element_condition(self):
        """A negated literal in a head aggregate element condition is lifted."""
        self.assertEqual(
            self._rewrite("1 <= #count{ X : p(X) : q(X), not r(X) } :- s."),
            [
                "1 <= #count { X: p(X): q(X), not RD1(X) } :- s.",
                "RD1(X) :- r(X).",
            ],
        )

    def test_lifts_head_aggregate_element_condition_with_variables(self):
        """The lifted head aggregate condition carries the literal's variables."""
        self.assertEqual(
            self._rewrite("2 = #sum{ Y,1 : p(Y) : q(Y), not r(Y,Z), t(Z) } :- u."),
            [
                "2 = #sum { Y,1: p(Y): q(Y), not RD1(Y,Z), t(Z) } :- u.",
                "RD1(Y,Z) :- r(Y,Z).",
            ],
        )

    def test_lifts_body_aggregate_element_condition(self):
        """A negated literal in a body aggregate element condition is lifted."""
        self.assertEqual(
            self._rewrite(":- #count{ X : p(X), not q(X) } > 5."),
            [" :- #count { X: p(X), not RD1(X) } > 5.", "RD1(X) :- q(X)."],
        )

    def test_lifts_body_aggregate_element_condition_nested_term(self):
        """The lifted literal keeps its nested argument terms in the aux rule."""
        self.assertEqual(
            self._rewrite("a :- #count{ X : p(X), not q(f(X)) } > 0."),
            [
                "a :- #count { X: p(X), not RD1(X) } > 0.",
                "RD1(X) :- q(f(X)).",
            ],
        )

    def test_lifts_body_set_aggregate_element_condition(self):
        """A negated literal in a body set aggregate condition is lifted."""
        self.assertEqual(
            self._rewrite("b :- 1 { p(X) : q(X), not r(X) }."),
            ["b :- 1 <= { p(X): q(X), not RD1(X) }.", "RD1(X) :- r(X)."],
        )

    def test_lifts_body_set_aggregate_with_multiple_elements(self):
        """Each body set aggregate element condition is lifted independently."""
        self.assertEqual(
            self._rewrite(":- { a : not b(Y), c(Y); d : not e } 0."),
            [
                " :- { a: not RD1(Y), c(Y); d: not RD2 } <= 0.",
                "RD1(Y) :- b(Y).",
                "RD2 :- e.",
            ],
        )

    def test_non_rule_statement_unchanged(self):
        """A non-rule statement is returned unchanged."""
        self._assert_unchanged(parse_string(self.lib, "a :- d.")[0].original)

    def test_rule_without_conditional_literals_unchanged(self):
        """A rule whose body has no conditional literals is unchanged."""
        self._assert_unchanged(parse_string(self.lib, "a :- d, not e.")[1].original)

    def test_positive_condition_unchanged(self):
        """A conditional literal without negated literals is unchanged."""
        self._assert_unchanged(
            parse_string(self.lib, "a :- b(X) : c(X), d(X).")[1].original
        )

    def test_negated_comparison_unchanged(self):
        """Negated comparisons in conditions are left untouched."""
        self._assert_unchanged(
            parse_string(self.lib, "a :- b(X) : c(X), not X = 3.")[1].original
        )

    def test_positive_aggregate_conditions_unchanged(self):
        """Aggregates and choices without negated condition literals are unchanged."""
        self._assert_unchanged(
            parse_string(self.lib, "{ p(X) : q(X) } :- s(X).")[1].original
        )
        self._assert_unchanged(
            parse_string(self.lib, ":- #count{ X : p(X), q(X) } > 5.")[1].original
        )
        self._assert_unchanged(
            parse_string(self.lib, "b :- 1 { p(X) : q(X) }.")[1].original
        )

    def test_bare_disjunction_unchanged(self):
        """A disjunction without conditional disjuncts is unchanged."""
        self._assert_unchanged(parse_string(self.lib, "a; not b :- d.")[1].original)

    def test_negated_aggregate_element_literal_unchanged(self):
        """A function-free negated aggregate element literal is not lifted."""
        self._assert_unchanged(
            parse_string(self.lib, "{ not p(X) : q(X) } :- s(X).")[1].original
        )

    def test_double_negation_unchanged(self):
        """Doubly negated condition literals are left untouched."""
        self._assert_unchanged(
            parse_string(self.lib, "a :- b(X) : c(X), not not d(X).")[1].original
        )

    def test_double_negation_in_aggregate_condition_unchanged(self):
        """Doubly negated literals in aggregate conditions are untouched."""
        self._assert_unchanged(
            parse_string(self.lib, ":- #count{ X : p(X), not not q(X) } > 5.")[
                1
            ].original
        )

    def test_double_negation_only_condition_unchanged(self):
        """A condition consisting only of a doubly negated literal is untouched."""
        self._assert_unchanged(
            parse_string(self.lib, "a :- b : not not c.")[1].original
        )

    def test_double_negation_only_condition_with_variables_unchanged(self):
        """A doubly negated condition literal with variables is untouched."""
        self._assert_unchanged(
            parse_string(self.lib, "a(X) :- b(X) : not not c(X,Y).")[1].original
        )

    def test_lifts_double_negated_intensional_condition_literal(self):
        """A doubly negated intensional condition literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("a :- b(X) : c(X), not not q(f(X))."),
            ["a :- b(X): c(X), not not RD1(X).", "RD1(X) :- q(f(X))."],
        )

    def test_lifts_double_negated_intensional_aggregate_condition(self):
        """A doubly negated intensional aggregate condition literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("a :- #count{ X : p(X), not not q(f(X)) } > 0."),
            [
                "a :- #count { X: p(X), not not RD1(X) } > 0.",
                "RD1(X) :- q(f(X)).",
            ],
        )

    def test_lifts_negated_intensional_element_literal(self):
        """A negated intensional aggregate element literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("1 = #count{ X : not p(f(X)) : q(X) } :- r."),
            [
                "1 = #count { X: not RD1(X): q(X) } :- r.",
                "RD1(X) :- p(f(X)).",
            ],
        )

    def test_lifts_double_negated_intensional_element_literal(self):
        """A doubly negated intensional element literal keeps its sign."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("1 = #count{ X : not not p(f(X)) : q(X) } :- r."),
            [
                "1 = #count { X: not not RD1(X): q(X) } :- r.",
                "RD1(X) :- p(f(X)).",
            ],
        )

    def test_lifts_negated_intensional_set_aggregate_element_literal(self):
        """A negated intensional set-aggregate element literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 0)}
        )
        self.assertEqual(
            self._rewrite(":- 1 { not p(f) : q }."),
            [" :- 1 <= { not RD1: q }.", "RD1 :- p(f)."],
        )

    def test_lifts_negated_intensional_weak_constraint_literal(self):
        """A negated intensional weak-constraint body literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":~ p(X), not q(f(X)). [1@0,X]"),
            [" :~ p(X); not RD1(X). [1@0,X]", "RD1(X) :- q(f(X))."],
        )

    def test_lifts_double_negated_intensional_weak_constraint_literal(self):
        """A doubly negated intensional weak-constraint literal keeps its sign."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":~ p(X), not not q(f(X)). [1@0,X]"),
            [" :~ p(X); not not RD1(X). [1@0,X]", "RD1(X) :- q(f(X))."],
        )

    def test_lifts_condition_in_weak_constraint_body(self):
        """Condition literals of weak-constraint body elements are lifted."""
        self.assertEqual(
            self._rewrite(":~ p(X); r(X) : s(X), not q(X). [1@0,X]"),
            [" :~ p(X); r(X): s(X), not RD1(X). [1@0,X]", "RD1(X) :- q(X)."],
        )

    def test_function_free_weak_constraint_body_unchanged(self):
        """A function-free negated weak-constraint body literal is untouched."""
        self._assert_unchanged(
            parse_string(self.lib, ":~ p(X), not q(X). [1@0,X]")[1].original
        )

    def test_lifts_negated_intensional_optimize_condition(self):
        """A negated intensional optimize condition literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("#minimize { 1,X : p(X), not q(f(X)) }."),
            ["#minimize { 1,X: p(X), not RD1(X) }.", "RD1(X) :- q(f(X))."],
        )

    def test_function_free_optimize_condition_unchanged(self):
        """A function-free negated optimize condition literal is untouched."""
        self._assert_unchanged(
            parse_string(self.lib, "#minimize { 1,X : p(X), not q(X) }.")[1].original
        )

    def test_lifts_negated_intensional_comparison_in_weak_body(self):
        """A negated intensional comparison in a weak body is lifted with guards."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":~ p(X), not f(X)+1 = 3. [1@0,X]"),
            [" :~ p(X); not RD1(X). [1@0,X]", "RD1(X) :- p(X); f(X)+1=3."],
        )

    def test_aggregate_guards_negated_comparison_in_weak_body(self):
        """Positive aggregates guard a lifted weak-body comparison."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":~ X = #count{ Y : p(Y) }, not f(X)+1 = 3. [1@0,X]"),
            [
                " :~ X = #count { Y: p(Y) }; not RD1(X). [1@0,X]",
                "RD1(X) :- X = #count { Y: p(Y) }; f(X)+1=3.",
            ],
        )

    def test_lifts_negated_intensional_comparison_in_optimize_condition(self):
        """A negated intensional comparison in an optimize condition is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("#minimize { 1,X : p(X), not f(X)+1 = 3 }."),
            ["#minimize { 1,X: p(X), not RD1(X) }.", "RD1(X) :- p(X); f(X)+1=3."],
        )

    def test_lifts_negated_intensional_comparison_in_aggregate_condition(self):
        """A negated intensional comparison in an aggregate condition is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":- 0 < #count{ X : p(X), not f(X)+1 = 3 }."),
            [
                " :- 0 < #count { X: p(X), not RD1(X) }.",
                "RD1(X) :- p(X); f(X)+1=3.",
            ],
        )

    def test_lifts_negated_intensional_comparison_in_condition(self):
        """A negated intensional comparison in a conditional literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("a :- q(X) : p(X), not f(X)+1 = 3."),
            ["a :- q(X): p(X), not RD1(X).", "RD1(X) :- p(X); f(X)+1=3."],
        )

    def test_lifts_negated_intensional_comparison_element_literal(self):
        """A negated intensional comparison as an element literal is lifted."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("1 = #count{ X : not f(X)+1 = 3 : q(X) } :- r."),
            [
                "1 = #count { X: not RD1(X): q(X) } :- r.",
                "RD1(X) :- r; q(X); f(X)+1=3.",
            ],
        )

    def test_rule_body_literals_guard_lifted_comparison(self):
        """Globals of a lifted comparison are guarded by rule-body literals."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":- q(Y), 0 < #count{ X : p(X), not f(X)+Y = 3 }."),
            [
                " :- q(Y); 0 < #count { X: p(X), not RD1(X,Y) }.",
                "RD1(X,Y) :- q(Y); p(X); f(X)+Y=3.",
            ],
        )

    def test_rule_body_literals_guard_conditional_literal_comparison(self):
        """Rule-body literals also guard comparisons in conditional literals."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("a :- q(Y); b(X) : c(X), not f(X)+Y = 3."),
            [
                "a :- q(Y); b(X): c(X), not RD1(X,Y).",
                "RD1(X,Y) :- q(Y); c(X); f(X)+Y=3.",
            ],
        )

    def test_rule_body_literals_guard_head_condition_comparison(self):
        """Rule-body literals also guard comparisons in head conditions."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite("a(X) : b(X), not f(X)+Y = 3 :- d(X), q(Y)."),
            [
                "a(X): b(X), not RD1(X,Y) :- d(X); q(Y).",
                "RD1(X,Y) :- d(X); q(Y); b(X); f(X)+Y=3.",
            ],
        )

    def test_weak_aggregate_is_not_copied_as_guard(self):
        """A weak-body aggregate never guards its own lifted condition.

        Regression test: the enclosing aggregate used to be copied into the
        auxiliary rule, guarding the very condition being lifted.
        """
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self.assertEqual(
            self._rewrite(":~ 0 < #count{ X : p(X), not f(X)+1 = 3 }. [1@0]"),
            [
                " :~ 0 < #count { X: p(X), not RD1(X) }. [1@0]",
                "RD1(X) :- p(X); f(X)+1=3.",
            ],
        )

    def test_plain_negated_equality_in_condition_unchanged(self):
        """A negated equality needing no unnesting is left to prefixing."""
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )
        self._assert_unchanged(
            parse_string(self.lib, ":- 0 < #count{ X : p(X,Y), not f(X) = Y }.")[
                1
            ].original
        )

    def test_auxiliary_prefix_avoids_function_prefix(self):
        """Auxiliary predicates do not start with the configured function prefix."""
        self.context = RewriteContext(self.lib, prefix_function="R")
        self.assertEqual(
            self._rewrite("a :- b(X) : c(X), not d(X)."),
            ["a :- b(X): c(X), not AD1(X).", "AD1(X) :- d(X)."],
        )

    def test_auxiliary_prefix_avoids_extending_function_prefix(self):
        """Auxiliary prefixes that the function prefix extends are avoided.

        With function prefix ``RD1``, auxiliary names ``RD1``, ``RD2``, ...
        would collide with function atoms, so ``AD`` must be used instead.
        """
        self.context = RewriteContext(self.lib, prefix_function="RD1")

        self.assertEqual(self.context.fresh_predicate_name(), "AD1")

    def test_auxiliary_prefix_raises_without_safe_prefix(self):
        """A missing safe auxiliary prefix is reported explicitly."""
        self.context = RewriteContext(self.lib, prefix_function="")

        with self.assertRaisesRegex(
            ValueError, "could not find an auxiliary predicate prefix"
        ):
            self.context.fresh_predicate_name()


class TestRewriteDoubleNegatedBodyLiterals(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()
        self.context = RewriteContext(
            self.lib, intensional_functions={SymbolSignature("f", 1)}
        )

    def _rewrite(self, program: str, index: int = 1) -> list[str]:
        """Rewrite a single rule and return the resulting statement strings."""
        statement = parse_string(self.lib, program)[index].original
        return [
            str(result)
            for result in rewrite_double_negated_body_literals(self.context, statement)
        ]

    def _assert_unchanged(self, program: str, index: int = 1):
        """Assert the parsed statement is returned unchanged as a singleton."""
        statement = parse_string(self.lib, program)[index].original
        result = rewrite_double_negated_body_literals(self.context, statement)
        self.assertEqual(len(result), 1)
        self.assertIs(result[0], statement)

    def test_lifts_double_negated_intensional_literal(self):
        """A doubly negated literal over an intensional function is lifted."""
        self.assertEqual(
            self._rewrite("a :- p(X), not not q(f(X))."),
            ["a :- p(X); not not RD1(X).", "RD1(X) :- q(f(X))."],
        )

    def test_other_body_literals_are_not_copied(self):
        """The auxiliary rule holds only the lifted literal itself."""
        self.assertEqual(
            self._rewrite("a :- p(X), not not q(f(X)), not r(X)."),
            [
                "a :- p(X); not not RD1(X); not r(X).",
                "RD1(X) :- q(f(X)).",
            ],
        )

    def test_ground_literal_yields_constant_auxiliary(self):
        """A doubly negated literal without variables yields a 0-ary auxiliary."""
        self.assertEqual(
            self._rewrite("b :- not not q(f(a))."),
            ["b :- not not RD1.", "RD1 :- q(f(a))."],
        )

    def test_function_free_literal_unchanged(self):
        """A doubly negated literal without intensional functions is untouched."""
        self._assert_unchanged("a :- p(X), not not q(X).")

    def test_double_negated_comparison_unchanged(self):
        """Doubly negated comparisons are not lifted."""
        self._assert_unchanged("a :- p(X), not not f(X) = 1.")

    def test_non_rule_statement_unchanged(self):
        """A non-rule statement is returned unchanged."""
        self._assert_unchanged("#show a/0.", index=0)


if __name__ == "__main__":
    unittest.main()
