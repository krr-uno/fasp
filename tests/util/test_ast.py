import unittest

from clingo import ast
from clingo.core import Location, Position, Library

from fasp.util.ast import SyntacticCheckVisitor, SyntacticError

INVALID_ASTTYPES = {
    ast.HeadSetAggregate,
    ast.BodyConditionalLiteral,
    ast.HeadDisjunction,
}


class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def setUp(self):
        self.lib = Library()

    def assertEqualErrors(self, program, expected_errors):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """

        syntactic_checker = SyntacticCheckVisitor(INVALID_ASTTYPES)

        def callback(statement):
            statement.visit(syntactic_checker)

        ast.parse_string(self.lib, program, callback)
        self.assertCountEqual(syntactic_checker.errors, expected_errors)

    def test_correct(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            #program actions.
            a :- b.
            b :- not c.
            c :- c.
            f = 1 :- g = h.
            f = 1 :- not g = h.
        """
        self.assertEqualErrors(program, [])

    def test_incorrect(self):
        """Test syntax checking with an incorrect program snippet."""
        program = """
            #program actions.
            a :- b : c.
            a, c :- b.
            {b} :- not c.
            c :- c.
            f = 1 :- g = h.
            f = 1 :- not g = h.
        """
        expected_errors = [
            SyntacticError(
                Location(
                    Position(self.lib, "<string>", 3, 18), Position(self.lib, "<string>", 3, 24)
                ),
                "unexpected b: c",
                ast.BodyConditionalLiteral,
            ),
            SyntacticError(
                Location(
                    Position(self.lib, "<string>", 4, 13), Position(self.lib, "<string>", 4, 20)
                ),
                "unexpected a; c",
                ast.HeadDisjunction,
            ),
            SyntacticError(
                Location(
                    Position(self.lib, "<string>", 5, 13), Position(self.lib, "<string>", 5, 16)
                ),
                "unexpected { b }",
                ast.HeadSetAggregate,
            ),
        ]
        self.maxDiff = None 
        self.assertEqualErrors(program, expected_errors)
