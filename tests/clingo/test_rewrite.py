import enum
from itertools import chain
import unittest

from clingo import ast
from clingo.core import Library


class TestRewrite(unittest.TestCase):

    def setUp(self):
        self.library = Library()

    def assertEqualRewritten(
        self,
        program,
        expected,
        expected_head_types=None,
        prerewrite_expected_head_types=None,
    ):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """
        rewrite_context = ast.RewriteContext(self.library)
        statements = []

        ast.parse_string(self.library, program, statements.append)

        if prerewrite_expected_head_types is not None:
            for i, (statement, expected_head_type) in enumerate(zip(
                statements[1:], prerewrite_expected_head_types
            )):
                self.assertIsInstance(statement, ast.StatementRule, f"Statement {i} is not a rule")
                self.assertIsInstance(statement.head, expected_head_type, f"Head of statement {i} is not of expected type {expected_head_type}")

        statements = list(
            chain.from_iterable(
                ast.rewrite_statement(rewrite_context, s) for s in statements
            )
        )

        self.assertCountEqual(
            set(map(lambda x: str(x).strip(), statements)),
            set(l2 for l in expected.splitlines() if (l2 := l.strip())),
        )
        if expected_head_types is not None:
            for statement, expected_head_type in zip(
                statements[1:], expected_head_types
            ):
                self.assertIsInstance(statement, ast.StatementRule, f"Statement {i} is not a rule")
                self.assertIsInstance(statement.head, expected_head_type, f"Head of statement {i} is not of expected type {expected_head_type}")

    def test_comparisons(self):
        program = """\
        X = Y :- X = Z, not Y = W, p(X, Y, Z, W).
        X != Y :- X != Z, not Y != W, p(X, Y, Z, W).
        X < Y :- X < Z, not Y < W, p(X, Y, Z, W).
        """
        expected = """\
        #program base.
        :- p(Z,Y,Z,W); Y!=W; Z!=Y.
        :- p(W,W,Z,W); W!=Z.
        :- p(X,Y,Z,W); X<Z; Y>=W; X>=Y.
        """
        self.maxDiff = None
        self.assertEqualRewritten(program, expected)

    def test_aggregates(self):
        program = """\
        { p(X,Y) } :- q(X,Y).
        { p(X,Y) = Z } :- q(X,Y,Z).
        #count { X,Y: p(X,Y) } :- q(X,Y).
        #count { X,Y: p(X,Y): r(X,Y) } :- q(X,Y).
        #sum { X,Y: p(X,Y) } :- q(X,Y).
        #sum { X,Y: p(X,Y): r(X,Y) } :- q(X,Y).
        """
        expected = """\
        #program base.
        #count { 0,p(X,Y): p(X,Y) } :- q(X,Y).
        #count { 3,X,Y,Z: #true: Z=p(X,Y) } :- q(X,Y,Z).
        #count { X,Y: p(X,Y) } :- q(X,Y).
        #count { X,Y: p(X,Y): r(X,Y) } :- q(X,Y).
        #sum { X,Y: p(X,Y) } :- q(X,Y).
        #sum { X,Y: p(X,Y): r(X,Y) } :- q(X,Y).
        """
        prerewrite_expected_head_types = [
            ast.HeadSetAggregate,
            ast.HeadSetAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
        ]
        expected_head_types = [
            ast.HeadAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
            ast.HeadAggregate,
        ]
        self.maxDiff = None
        self.assertEqualRewritten(program, expected, expected_head_types, prerewrite_expected_head_types)
