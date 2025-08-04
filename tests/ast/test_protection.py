import textwrap
import unittest

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.ast import RewriteContext
from fasp.util.ast import AST

from fasp.ast.protecting import (
    _ComparisonProtectorTransformer,
    _ComparisonRestorationTransformer,
    _restore_guard_arguments,
    protect_comparisons,
    restore_comparison,
)


def _restore_guard(library: Library, term: ast.TermFunction) -> ast.RightGuard:
    position = Position(library, "<aux>", 0, 0)
    location = Location(position, position)
    right = _restore_guard_arguments(location, term)
    return ast.RightGuard(library, right.relation, right.term)


class TestProtectComparisons(unittest.TestCase):
    """ """

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

    def test_basic(self):
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

    def test_aggregates(self):
        """Test syntax checking with a correct program snippet."""
        program = """\
            {f = 100} = 1 :- #count{ X,Y : g = X, not h = Y } > 5.
            #count { X : f(X) = Y : p(X) } :- q(Y).
            #sum { X : f(X) = Y : p(X) } :- q(Y).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            { CMP(f,(GRD(0,100),),0) } = 1 :- #count { X,Y: CMP(g,(GRD(0,X),),0), CMP(h,(GRD(0,Y),),1) } > 5.
            #count { X: CMP(f(X),(GRD(0,Y),),0): p(X) } :- q(Y).
            #sum { X: CMP(f(X),(GRD(0,Y),),0): p(X) } :- q(Y).
        """
        ).strip()
        self.assertEqualRewrite(program, expected)


class TestRestoreComparisons(unittest.TestCase):
    """ """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.lib = Library()
        self.rewrite_context = RewriteContext(self.lib)

    def test_restore(self):
        cmp = ast.parse_literal(self.lib, "a=100")
        self.assertEqual(str(cmp), "a=100")
        transformer = _ComparisonProtectorTransformer(self.lib)
        protected = transformer.dispatch(cmp)
        self.assertEqual(str(protected), "CMP(a,(GRD(0,100),),0)")
        guard = protected.atom.pool[0].arguments[1].pool[0].arguments[0]
        self.assertEqual(str(guard), "GRD(0,100)")
        self.assertIsInstance(guard, ast.TermFunction)
        restored_guard = _restore_guard(self.lib, guard)
        self.assertEqual(str(restored_guard), " = 100")
        restored = restore_comparison(self.lib, protected)
        self.assertEqual(str(restored), "a=100")
        self.assertIsInstance(restored, ast.LiteralComparison)

    def test_restore_symbolic(self):
        lit = ast.parse_literal(self.lib, "#true")
        transformer = _ComparisonProtectorTransformer(self.lib)
        protected = transformer.dispatch(lit)
        restorer = _ComparisonRestorationTransformer(self.lib)
        restored = restorer.dispatch(protected)
        self.assertEqual(lit, restored)
