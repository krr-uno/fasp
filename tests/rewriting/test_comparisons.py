"""Tests for functional-comparison rewriting boundaries."""

import unittest

from clingo_funasp import ast, symbol
from clingo_funasp.core import Location, Position

from funasp.ast._rewritings.comparisons import ComparisonTransformer
from funasp.core import Library


class TestComparisonTransformer(unittest.TestCase):
    def test_non_function_symbolic_assignment_is_rejected(self):
        """The term builder enforces its function-valued input contract."""
        library = Library().library
        position = Position(library, "<test>", 1, 1)
        location = Location(position, position)
        assigned = ast.TermSymbolic(library, location, symbol.Number(library, 1))
        value = ast.TermSymbolic(library, location, symbol.Number(library, 2))
        transformer = ComparisonTransformer(library, set())

        with self.assertRaisesRegex(
            TypeError,
            "assigned function must be a function-valued TermFunction or TermSymbolic",
        ):
            transformer._build_intensional_function_to_term(assigned, value, location)


if __name__ == "__main__":
    unittest.main()
