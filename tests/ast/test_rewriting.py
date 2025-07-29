from math import exp
import textwrap
from turtle import st
import unittest

from clingo import ast
from fasp.util.ast import Library

from fasp.ast.rewriting import functional2asp



class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.lib = Library()

    def assertEqualRewrite(self, program, expected):
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

        _, result = functional2asp(self.lib, statements)

        expected_lines = [line.strip() for line in expected.splitlines()]

        self.assertCountEqual(list(map(str, result)), expected_lines)
        for statement in result:
            self.assertIsInstance(statement, ast.AST)

    def test_correct(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            { d }.
            a :- b.
            b :- not c.
            c :- c.
            f = X :- X = 1; g = h.
            f = X :- X = 1; not g = h.
            f2(X) = Y :- p(X,Y).
            f3(X) = a :- p(X).
            f4(X) = 5 :- p(X).
            f3(X) = a(b,5) :- p(X).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            { d }.
            a :- b.
            b :- not c.
            c :- c.
            Ff(X) :- X = 1; g = h.
            Ff(X) :- X = 1; not g = h.
            Ff2(X,Y) :- p(X,Y).
            Ff3(X,a) :- p(X).
            Ff4(X,5) :- p(X).
            Ff3(X,a(b,5)) :- p(X).
            #false :- Ff(_); 1 > #count { Ff(V) }.
            #false :- Ff2(X0,_); 1 > #count { Ff2(X0,V) }.
            #false :- Ff3(X0,_); 1 > #count { Ff3(X0,V) }.
            #false :- Ff4(X0,_); 1 > #count { Ff4(X0,V) }.
        """
        ).strip()
        self.assertEqualRewrite(program, expected)
