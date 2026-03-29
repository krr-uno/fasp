import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from asp2funasp.transformers.preprocessing.constraint_aggregate_guard_normalization import (
    ConstraintAggregateGuardTransformer
)

from asp2funasp.util.ast import AST

from tests.util import collect_statements

class ConstraintAggregateGuardTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = ConstraintAggregateGuardTransformer(self.lib)

    def _apply(self, program: str) -> str:
        """Parse `program`, rewrite each rule, and return normalized text."""
        program = textwrap.dedent(program).strip()
        nodes: list[ast.StatementRule] = collect_statements(self.lib, program)

        rewritten: list[str] = []
        for node in nodes:
            new_node = self.transformer.rewrite_rule(node) or node
            rewritten.append(str(new_node).strip())

        return "\n".join(line for line in rewritten if line)

    def assertRewriteEqual(self, program: str, expected: str) -> None:
        result = self._apply(program)
        expected = textwrap.dedent(expected).strip()
        self.assertEqual(result, expected)

    # -----------------------------------------------------------
    # TESTS
    # -----------------------------------------------------------

    def test_left_guard_strict_less_is_normalized(self) -> None:
        self.assertRewriteEqual(
            """
            :- 3 < { a(X) }.
            """,
            """
            :- 4 <= { a(X) }.
            """,
        )

    def test_variable_in_guard_with_less_than_relation_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            :- Y <= { a(X) } < X.
            """,
            """
            :- Y <= { a(X) } < X.
            """)
    def test_inequality_right_guard_moves_to_left(self) -> None:
        self.assertRewriteEqual(
            """
            :- { a(X) } != 1.
            """,
            """
            :- 1 != { a(X) }.
            """)


    ## EXTRA TESTS ##
    def test_right_guard_strict_less_is_normalized(self) -> None:
        self.assertRewriteEqual(
            """
            :- { a(X) } < 5.
            """,
            """
            :- { a(X) } <= 4.
            """,
        )
