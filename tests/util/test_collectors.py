"""
Unit tests for ``funasp.util.collectors``.
"""

import unittest

from clingo_funasp import symbol

from funasp.ast import parse_string
from funasp.core import Library
from funasp.util.collectors import collect_predicates, collect_variables_ordered


class TestCollectPredicates(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def _predicates(self, program: str) -> set[str]:
        """Return the string signatures of the predicates in a single rule."""
        statement = parse_string(self.lib, program)[1].original
        return {str(signature) for signature in collect_predicates(statement)}

    def test_disjunctive_head_and_negated_body(self):
        """Head disjunction and negated body literals are all collected."""
        self.assertEqual(
            self._predicates("a, b(X) :- c(d,X), not e, not f(7)."),
            {"a/0", "b/1", "c/2", "e/0", "f/1"},
        )

    def test_conditional_literals(self):
        """Conditions of head and body conditional literals are collected."""
        self.assertEqual(
            self._predicates("a : b(X) :- c(d,X), e(Y) : f(Y)."),
            {"a/0", "b/1", "c/2", "e/1", "f/1"},
        )

    def test_aggregate_condition(self):
        """Predicates inside an aggregate element condition are collected."""
        self.assertEqual(
            self._predicates(":- a(X), #count{ Y : b(X,Y)} > 5."),
            {"a/1", "b/2"},
        )

    def test_non_function_symbolic_atom(self):
        """A symbolic atom that is not a function contributes no predicate."""
        statement = parse_string(self.lib, "g.")[1].original
        head = statement.head
        literal = head.literal
        atom = literal.atom.update(
            self.lib.library, symbol=symbol.Number(self.lib.library, 7)
        )
        literal = literal.update(self.lib.library, atom=atom)
        head = head.update(self.lib.library, literal=literal)
        statement = statement.update(self.lib.library, head=head)
        self.assertEqual(collect_predicates(statement), set())

    def test_pooled_atom(self):
        """A pooled predicate contributes one signature per argument tuple."""
        self.assertEqual(
            self._predicates("p(1;2,3) :- q."),
            {"p/1", "p/2", "q/0"},
        )


class TestCollectVariablesOrdered(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def _variables(self, program: str) -> list[str]:
        """Return the ordered variable names of a single rule."""
        statement = parse_string(self.lib, program)[1].original
        return collect_variables_ordered(statement)

    def test_order_of_first_occurrence(self):
        """Variables are returned in order of first occurrence."""
        self.assertEqual(self._variables("p(Y,X) :- q(X,Z,Y)."), ["Y", "X", "Z"])

    def test_duplicates_collapsed(self):
        """Repeated occurrences of a variable are collapsed."""
        self.assertEqual(self._variables("p(X,g(X,Y),X)."), ["X", "Y"])

    def test_no_variables(self):
        """A ground rule yields no variables."""
        self.assertEqual(self._variables("p(1,a)."), [])


if __name__ == "__main__":
    unittest.main()
