from math import exp
import textwrap
from turtle import st
import unittest

from clingo import ast
from clingo.core import Library
from clingo.ast import RewriteContext, rewrite_statement
from fasp.util.ast import AST

from fasp.ast.protecting import protect_comparisons


class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.lib = Library()
        self.rewrite_context = RewriteContext(self.lib)

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

        result = protect_comparisons(self.lib, statements)

        expected_lines = [line.strip() for line in expected.splitlines()]

        self.maxDiff = None
        self.assertCountEqual(
            list(map(lambda x: str(x).strip(), result)), expected_lines
        )
        for statement in result:
            self.assertIsInstance(statement, AST)

    def test_correct(self):
        """Test syntax checking with a correct program snippet."""
        program = """\
            f = 100 :- not g = 100.
            f(X) > 100 :- 1000 < g(X,Y) < 1010.
            f(a,5) >= 100 :- g("string") <= 100, not h(3+5) != 300, not not z = p.
        """
        expected = textwrap.dedent(
            """\
            #program base.
            CMP(f,(GRD(0,100),),0) :- CMP(g,(GRD(0,100),),1).
            CMP(f(X),(GRD(1,100),),0) :- CMP(1000,(GRD(3,g(X,Y)),GRD(3,1010)),0).
            CMP(f(a,5),(GRD(2,100),),0) :- CMP(g("string"),(GRD(4,100),),0); CMP(h(3+5),(GRD(5,300),),1); CMP(z,(GRD(0,p),),2).
        """
        ).strip()
        self.assertEqualRewrite(program, expected)

    # def test_symbol_argument(self):
    #     """Test syntax checking with a correct program snippet."""
    #     program = """\
    #         f0 = X :- X=1; g=h.
    #         f1(a) = X :- X=1; not g=h.
    #         f1(5) = X :- X=1; not g=h.
    #         f2(a,b) = X :- X=1; not g=h.
    #     """
    #     expected = textwrap.dedent(
    #         """\
    #         #program base.
    #         Ff0(X) :- X=1; g=h.
    #         Ff1(a,X) :- X=1; not g=h.
    #         Ff1(5,X) :- X=1; not g=h.
    #         Ff2(a,b,X) :- X=1; not g=h.
    #         :- Ff0(_); 1 > #count { V: Ff0(V) }.
    #         :- Ff1(X0,_); 1 > #count { V: Ff1(X0,V) }.
    #         :- Ff2(X0,X1,_); 1 > #count { V: Ff2(X0,X1,V) }.
    #     """
    #     ).strip()
    #     self.assertEqualRewrite(program, expected)
