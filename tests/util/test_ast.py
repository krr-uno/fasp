import unittest

from clingo import ast
from clingo.core import Location, Position, Library

from fasp.util.ast import SyntacticCheckVisitor, SyntacticError
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


class TestFreshVariableManager(unittest.TestCase):
    """Tests FreshVariableManager for creating fresh variables."""

    def setUp(self):
        self.lib = Library()
        self.loc = Location(
            Position(self.lib, "<stdin>", 1, 1),
            Position(self.lib, "<stdin>", 1, 1),
        )
        # Reset state before every test
        from fasp.util import ast as util_ast
        util_ast.FreshVariableManager._used.clear()
        util_ast.FreshVariableManager._initialized = False
        self.mgr = util_ast.FreshVariableManager
        self.ast = ast

    def parse_program(self, program: str):
        stmts = []
        self.ast.parse_string(self.lib, program, stmts.append)
        return stmts

    def test_collect_vars_only_once(self):
        """collect_vars should do nothing on repeated calls."""
        stmts = self.parse_program("p(X). q(Y).")
        self.mgr.collect_vars(stmts)
        used_first = set(self.mgr._used)
        # Second call should be a no-op
        self.mgr.collect_vars(stmts)
        self.assertEqual(used_first, self.mgr._used)

    def test_collect_vars_handles_none_and_lists(self):
        """_collect_vars should handle None and lists."""
        # Call private method directly with None
        self.mgr._collect_vars(None)  # should not raise

        # A variable inside a list
        term_var = self.ast.TermVariable(self.lib, self.loc, "X", False)
        self.mgr._collect_vars([term_var])  # should recurse
        self.assertIn("X", self.mgr._used)

    def test_fresh_variable_requires_collect_first(self):
        """fresh_variable should raise if not initialized."""
        with self.assertRaises(RuntimeError):
            self.mgr.fresh_variable(self.lib, self.loc, base="X")

    def test_fresh_variable_normal_and_numbered(self):
        """fresh_variable should generate unused names sequentially."""
        stmts = self.parse_program("p(X,Y).")
        self.mgr.collect_vars(stmts)

        v1 = self.mgr.fresh_variable(self.lib, self.loc, base="X")
        v2 = self.mgr.fresh_variable(self.lib, self.loc, base="X")
        v3 = self.mgr.fresh_variable(self.lib, self.loc, base="Z")

        self.assertEqual(v1.name, "X1")
        self.assertEqual(v2.name, "X2")
        self.assertEqual(v3.name, "Z")

    def test_exhaustion_runtime_error(self):
        """Force exhaustion path."""
        self.mgr._initialized = True
        # Pre-fill _used with base and all candidates up to some small number
        self.mgr._used = {f"X{i}" for i in range(1, 10)}
        self.mgr._used.add("X")  # also block bare "X"

        # Set sys.maxsize down so loop terminates quickly for testing
        import sys
        orig_max = sys.maxsize
        sys.maxsize = 10
        try:
            with self.assertRaises(RuntimeError):
                self.mgr.fresh_variable(self.lib, self.loc, base="X")
        finally:
            sys.maxsize = orig_max