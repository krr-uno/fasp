import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from asp2fasp.transformers.preprocessing.aggregate_head_body_condition_rewrite import (
    AggregateHeadBodyConditionTransformer,
)

from asp2fasp.util.ast import AST
class AggregateHeadBodyConditionTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = AggregateHeadBodyConditionTransformer(self.lib)

    def _apply(self, program: str) -> str:
        """Parse `program`, rewrite each rule, and return the normalized text."""
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)

        rewritten: list[str] = []
        for node in nodes:
            new_node = self.transformer.rewrite_rule(node) or node
            rewritten.append(str(new_node).strip())
        rewritten =rewritten[1:]
        return "\n".join(line for line in rewritten if line)

    def assertRewriteEqual(self, program: str, expected: str) -> None:
        result = self._apply(program)
        expected = textwrap.dedent(expected).strip()
        self.assertEqual(result, expected)

    def test_moves_conditions_from_head_aggregate(self) -> None:
        self.assertRewriteEqual(
            """
            { a(X,Y) : b(X); c(X,Y) : d(X) } :- e(Y).
            """,
            """
            { a(X,Y); c(X,Y) } :- e(Y); b(X); d(X).
            """,
        )

    def test_multiple_elements_conditions_are_lifted(self) -> None:
        self.assertRewriteEqual(
            """
            #count { 0,a(X,Y): a(X,Y): b(X); 0,c(X,Y): c(X,Y): d(X) } :- e(Y).
            """,
            """
            #count { 0,a(X,Y): a(X,Y); 0,c(X,Y): c(X,Y) } :- e(Y); b(X); d(X).
            """,
        )

    def test_guarded_head_aggregate_remains_unchanged(self) -> None:
        program = """
            1 <= { a(X): cond(X) } <= 1 :- base(X).
        """
        self.assertRewriteEqual(program, program)

    def test_head_without_conditions_is_unchanged(self) -> None:
        program = """
            { a(X) } :- base(X).
        """
        self.assertRewriteEqual(program, program)

    def test_head_without_aggregate_is_unchanged(self) -> None:
        program = """
            a(X) :- base(X).
        """
        self.assertRewriteEqual(program, program)