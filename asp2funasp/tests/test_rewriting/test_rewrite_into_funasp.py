from typing import List
import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from funasp.util.ast import AST
from asp2funasp.util.types import FRelation
from asp2funasp.rewriting.rewrite_into_funasp import (
    FunctionalBodyRewriteTransformer,
)

from tests.util import collect_statements


class FunctionalBodyRewriteTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    # APPLY TRANSFORMER
    def _rewrite(
        self,
        program: str,
        frels: List[FRelation],
    ) -> List[AST]:
        program = textwrap.dedent(program).strip()

        nodes: List[AST] = collect_statements(self.lib, program)

        transformer = FunctionalBodyRewriteTransformer(self.lib, frels)

        new_nodes: List[AST] = []

        for node in nodes:
            new_node = transformer.transform_rule(node)

            if new_node is None:
                new_nodes.append(node)
            else:
                new_nodes.append(new_node)

        return new_nodes

    # ASSERT HELPER
    def assertEqualRewrite(
        self,
        program: str,
        expected: str,
        frels: List[FRelation],
    ):
        actual_nodes = self._rewrite(program, frels)
        expected_nodes = collect_statements(
            self.lib,
            textwrap.dedent(expected).strip(),
        )

        # Compare via string form (standard in clingo AST tests)
        actual_str = "\n".join(str(n) for n in actual_nodes)
        expected_str = "\n".join(str(n) for n in expected_nodes)

        self.assertEqual(
            actual_str,
            expected_str,
            msg=f"\nEXPECTED:\n{expected_str}\n\nACTUAL:\n{actual_str}",
        )

    ## TESTS ##

    def test_no_functions(self):
        program = ":- a."
        expected = ":- a."
        frels = []

        self.assertEqualRewrite(program, expected, frels)

    def test_simple_body_rewrite(self):
        program = """
        :- assign(N,C), node(N).
        """

        expected = """
        :- assign(N) = C, node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_aggregate_rewrite(self):
        program = """
        :- #count{ C,N : assign(N,C) } != 1, node(N).
        """

        expected = """
        :- #count{ C,N : assign(N) = C } != 1, node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_no_rewrite_if_not_functional(self):
        program = """
        :- assign(N,C), node(N), a.
        """

        expected = """
        :- assign(N,C), node(N), a.
        """

        frels: List[FRelation] = []  # nothing functional

        self.assertEqualRewrite(program, expected, frels)

    def test_multiple_args_function(self):
        program = """
        :- f(X,Y,Z).
        """

        expected = """
        :- f(X,Y) = Z.
        """

        frels = [
            FRelation(
                name="f",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_negated_literal(self):
        program = """
        :- not assign(N,C).
        """

        expected = """
        :- not assign(N) = C.
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)