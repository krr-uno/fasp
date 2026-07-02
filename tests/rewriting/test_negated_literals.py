"""
Unit tests for ``funasp.ast._rewritings.negated_literals.rewrite_negated_head_literals``.
"""

import unittest

from funasp.ast import RewriteContext, parse_string
from funasp.ast._rewritings.negated_literals import rewrite_negated_head_literals
from funasp.core import Library


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


if __name__ == "__main__":
    unittest.main()
