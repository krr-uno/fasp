from itertools import chain
from re import S
import unittest

from clingo import ast
from clingo.core import Location, Position, Library

from fasp.util.ast import SyntacticCheckVisitor, SyntacticError
from fasp.ast.rewriting.collectors import collect_variables, VariableCollector
from fasp.util import ast as util_ast


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
                    Position(self.lib, "<string>", 3, 18),
                    Position(self.lib, "<string>", 3, 24),
                ),
                "unexpected b: c",
                ast.BodyConditionalLiteral,
            ),
            SyntacticError(
                Location(
                    Position(self.lib, "<string>", 4, 13),
                    Position(self.lib, "<string>", 4, 20),
                ),
                "unexpected a; c",
                ast.HeadDisjunction,
            ),
            SyntacticError(
                Location(
                    Position(self.lib, "<string>", 5, 13),
                    Position(self.lib, "<string>", 5, 16),
                ),
                "unexpected { b }",
                ast.HeadSetAggregate,
            ),
        ]
        self.maxDiff = None
        self.assertEqualErrors(program, expected_errors)


class TestVariableManager(unittest.TestCase):
    """Tests VariableCollector and FreshVariableGenerator."""

    def setUp(self):
        self.lib = Library()
        self.loc = Location(
            Position(self.lib, "<stdin>", 1, 1),
            Position(self.lib, "<stdin>", 1, 1),
        )
        self.ast = ast

    def parse_program(self, program: str):
        stmts = []
        self.ast.parse_string(self.lib, program, stmts.append)
        return stmts

    # VariableCollector tests

    # FreshVariableGenerator tests

    def test_fresh_variable_simple_and_numbered(self):
        gen = util_ast.FreshVariableGenerator({"X"})
        v1 = gen.fresh_variable(self.lib, self.loc, name="X")
        v2 = gen.fresh_variable(self.lib, self.loc, name="X")
        v3 = gen.fresh_variable(self.lib, self.loc, name="Z")

        self.assertEqual(v1.name, "X2")
        self.assertEqual(v2.name, "X3")
        self.assertEqual(v3.name, "Z")

    def test_fresh_variable_with_empty_used(self):
        gen = util_ast.FreshVariableGenerator()
        v = gen.fresh_variable(self.lib, self.loc, name="Y")
        self.assertEqual(v.name, "Y")

    def test_generator_isolated_instances(self):
        gen1 = util_ast.FreshVariableGenerator({"X"})
        gen2 = util_ast.FreshVariableGenerator({"Y"})

        v1 = gen1.fresh_variable(self.lib, self.loc, "X")
        v2 = gen2.fresh_variable(self.lib, self.loc, "Y")

        self.assertTrue(v1.name.startswith("X2"))
        self.assertTrue(v2.name.startswith("Y2"))

    # VariableCollector and FreshVariableGenerator integration tests

    def test_pipeline_basic_program(self):
        """Collector should feed into generator with proper fresh variables."""
        stmts = self.parse_program("p(X,Y). q(Z).")
        used = set(chain.from_iterable(collect_variables(stmt) for stmt in stmts))
        self.assertEqual(used, {"X", "Y", "Z"})

        gen = util_ast.FreshVariableGenerator(used)
        v1 = gen.fresh_variable(self.lib, self.loc, "X")
        v2 = gen.fresh_variable(self.lib, self.loc, "Y")
        v3 = gen.fresh_variable(self.lib, self.loc, "W")

        self.assertEqual(v1.name, "X2")
        self.assertEqual(v2.name, "Y2")
        self.assertEqual(v3.name, "W")

    def test_pipeline_multiple_rules(self):
        """Variables across multiple rules should all be collected and respected."""
        stmts = self.parse_program("p(A). q(B,C). r(D,E,F).")
        used = set(chain.from_iterable(collect_variables(stmt) for stmt in stmts))
        self.assertEqual(used, {"A", "B", "C", "D", "E", "F"})

        gen = util_ast.FreshVariableGenerator(used)
        v1 = gen.fresh_variable(self.lib, self.loc, "A")
        v2 = gen.fresh_variable(self.lib, self.loc, "C")
        v3 = gen.fresh_variable(self.lib, self.loc, "G")

        self.assertEqual(v1.name, "A2")
        self.assertEqual(v2.name, "C2")
        self.assertEqual(v3.name, "G")
