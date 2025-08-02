import unittest

from clingo import ast
from clingo.core import Library

from fasp.ast.protecting import protect_comparisons
from fasp.ast.syntax_checking import get_evaluable_functions, ParsingException


class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def setUp(self):
        self.lib = Library()

    def assertEqualFunctions(self, program, expected):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """
        statements = []

        def callback(statement):
            statements.append(statement)

        ast.parse_string(self.lib, program, callback)

        statements = list(protect_comparisons(self.lib, statements))

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

    def test_incorrect_1(self):
        """Test syntax checking with an incorrect program snippet."""
        program = """\
            a :- b.
            b :- not c.
            c :- c.
            f > 1 :- g = h.
            f = 1 :- not g = h.
            f2(X) = 1 :- p(X).
            5 = f :- q(Y).
        """
        with self.assertRaises(ParsingException) as captured_stderr:
            self.assertEqualFunctions(program, ["f/0", "f2/1"])
        messages = map(
            lambda e: (e.location.begin.line, e.message),
            captured_stderr.exception.errors,
        )
        self.assertCountEqual(
            messages,
            [
                (
                    4,
                    "unexpected comparison f>1 in the head. Assignments are of the form 'FUNCTION = TERM'.",
                ),
                (
                    7,
                    "unexpected comparison 5=f in the head. Assignments are of the form 'FUNCTION = TERM'.",
                ),
            ],
        )

    def test_incorrect_2(self):
        """Test syntax checking with an incorrect program snippet."""
        program = """\
            a :- b.
            b :- not c.
            c :- c.
            f = 1 :- g = h.
            not f = 1 :- not g = h.
            f2(X) = 1 :- p(X).
            f = 5  :- q(Y).
        """
        with self.assertRaises(ParsingException) as captured_stderr:
            self.assertEqualFunctions(program, ["f/0", "f2/1"])
        messages = map(
            lambda e: (e.location.begin.line, e.message),
            captured_stderr.exception.errors,
        )
        self.assertCountEqual(
            messages,
            [
                (
                    5,
                    "unexpected negated comparison not f=1 in the head. Assignments are of the form 'FUNCTION = TERM'.",
                ),
            ],
        )
