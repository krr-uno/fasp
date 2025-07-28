import unittest

from clingo import ast

from clingox.ast import normalize_symbolic_terms

from fasp.ast.syntax_checking import get_evaluable_functions, ParsingException


class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def assertEqualFunctions(self, program, expected):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """
        statements = []

        def callback(statement):
            statement = normalize_symbolic_terms(statement)
            statements.append(statement)

        ast.parse_string(program, callback)

        evaluable_functions = get_evaluable_functions(statements)

        self.assertCountEqual(set(map(str, evaluable_functions)), set(expected))

    def test_correct(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            #program actions.
            a :- b.
            b :- not c.
            c :- c.
            f = 1 :- g = h.
            f = 1 :- not g = h.
            f2(X) = 1 :- p(X).
        """
        self.assertEqualFunctions(program, ["f/0", "f2/1"])

    def test_incorrect(self):
        """Test syntax checking with an incorrect program snippet."""
        program = """
            #program actions.
            a :- b.
            b :- not c.
            c :- c.
            f > 1 :- g = h.
            f = 1 :- not g = h.
            f2(X) = 1 :- p(X).
        """
        with self.assertRaises(ParsingException) as captured_stderr:
            self.assertEqualFunctions(program, ["f/0", "f2/1"])
        self.assertEqual(
            captured_stderr.exception.errors[0].message,
            "unexpected comparison f > 1 in the head. Assignments are of the form 'FUNCTION = TERM'.",
        )
