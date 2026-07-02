"""
Unit tests for the head- and condition-literal rewritings in
``funasp.ast._rewritings.negated_literals``.
"""

import unittest

from funasp.ast import RewriteContext, parse_string
from funasp.ast._rewritings.negated_literals import (
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

    def _rewrite(self, program: str) -> str:
        """Rewrite a single rule and return its string form."""
        statement = parse_string(self.lib, program)[1].original
        return str(rewrite_negated_condition_literals(self.context, statement))

    def _auxiliary(self) -> list[str]:
        """Return the string forms of the generated auxiliary rules."""
        return [str(statement) for statement in self.context.auxiliary_statements]

    def test_lifts_negated_condition_literal(self):
        """A negated condition literal is replaced by an auxiliary call."""
        self.assertEqual(
            self._rewrite("a :- b(X); c(X,Y) : d(Y), not e(5,f(Y;Y+2))."),
            "a :- b(X); c(X,Y): d(Y), not RD1(Y).",
        )
        self.assertEqual(self._auxiliary(), ["RD1(Y) :- e(5,f(Y;Y+2))."])

    def test_lifts_literal_with_two_variables_in_argument(self):
        """The auxiliary call carries the distinct variables of the literal."""
        self.assertEqual(
            self._rewrite("b(2) :- c(X) : d(X), not p(g(X,Y))."),
            "b(2) :- c(X): d(X), not RD1(X,Y).",
        )
        self.assertEqual(self._auxiliary(), ["RD1(X,Y) :- p(g(X,Y))."])

    def test_counter_increments_across_rules(self):
        """Each lifted literal gets the next auxiliary predicate name."""
        first = self._rewrite("a :- b(X) : c(X), not d(X).")
        second = self._rewrite("e :- b(X) : c(X), not f(X).")
        self.assertEqual(first, "a :- b(X): c(X), not RD1(X).")
        self.assertEqual(second, "e :- b(X): c(X), not RD2(X).")
        self.assertEqual(self._auxiliary(), ["RD1(X) :- d(X).", "RD2(X) :- f(X)."])

    def test_skips_used_predicate_names(self):
        """Names already used in the program are skipped by the generator."""
        self.context.predicates.add(SymbolSignature("RD1", 3))
        self.assertEqual(
            self._rewrite("a :- b(X) : c(X), not d(X)."),
            "a :- b(X): c(X), not RD2(X).",
        )
        self.assertEqual(self._auxiliary(), ["RD2(X) :- d(X)."])

    def test_zero_variable_literal(self):
        """A negated literal without variables yields a 0-ary auxiliary."""
        self.assertEqual(
            self._rewrite("q :- r(X) : s(X), not t(5)."),
            "q :- r(X): s(X), not RD1.",
        )
        self.assertEqual(self._auxiliary(), ["RD1 :- t(5)."])

    def test_anonymous_variables_are_projected(self):
        """Anonymous variables do not become auxiliary arguments."""
        self.assertEqual(
            self._rewrite("q :- r(X) : s(X), not t(X,_)."),
            "q :- r(X): s(X), not RD1(X).",
        )
        self.assertEqual(self._auxiliary(), ["RD1(X) :- t(X,_)."])

    def test_non_rule_statement_unchanged(self):
        """A non-rule statement is returned unchanged."""
        statement = parse_string(self.lib, "a :- d.")[0].original
        self.assertIs(
            rewrite_negated_condition_literals(self.context, statement), statement
        )

    def test_rule_without_conditional_literals_unchanged(self):
        """A rule whose body has no conditional literals is unchanged."""
        statement = parse_string(self.lib, "a :- d, not e.")[1].original
        self.assertIs(
            rewrite_negated_condition_literals(self.context, statement), statement
        )

    def test_positive_condition_unchanged(self):
        """A conditional literal without negated literals is unchanged."""
        statement = parse_string(self.lib, "a :- b(X) : c(X), d(X).")[1].original
        self.assertIs(
            rewrite_negated_condition_literals(self.context, statement), statement
        )

    def test_negated_comparison_unchanged(self):
        """Negated comparisons in conditions are left untouched."""
        statement = parse_string(self.lib, "a :- b(X) : c(X), not X = 3.")[1].original
        self.assertIs(
            rewrite_negated_condition_literals(self.context, statement), statement
        )

    def test_double_negation_unchanged(self):
        """Doubly negated condition literals are left untouched."""
        statement = parse_string(self.lib, "a :- b(X) : c(X), not not d(X).")[
            1
        ].original
        self.assertIs(
            rewrite_negated_condition_literals(self.context, statement), statement
        )


if __name__ == "__main__":
    unittest.main()
