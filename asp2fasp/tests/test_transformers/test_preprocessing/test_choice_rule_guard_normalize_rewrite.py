import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from asp2fasp.transformers.preprocessing.choice_rule_guard_normalize_rewrite import (
    ChoiceGuardTransformer,
)

from asp2fasp.util.ast import AST


class ChoiceGuardTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = ChoiceGuardTransformer(self.lib)

    def _apply(self, program: str) -> str:
        """Parse `program`, rewrite each rule, and return normalized text."""
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)

        rewritten: list[str] = []
        for node in nodes:
            new_node = self.transformer.rewrite_rule(node) or node
            rewritten.append(str(new_node).strip())

        rewritten = rewritten[1:]
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
            3 < { a(X) }.
            """,
            """
            4 <= { a(X) }.
            """,
        )

    def test_right_guard_strict_less_is_normalized(self) -> None:
        self.assertRewriteEqual(
            """
            { a(X) } < 5.
            """,
            """
            { a(X) } <= 4.
            """,
        )

    def test_both_sides_strict_less_are_normalized(self) -> None:
        self.assertRewriteEqual(
            """
            2 < { a(X) } < 6.
            """,
            """
            3 <= { a(X) } <= 5.
            """,
        )

    def test_equal_bounds_collapse_to_equality(self) -> None:
        self.assertRewriteEqual(
            """
            2 < { a(X) } < 4.
            """,
            """
            3 = { a(X) }.
            """,
        )

    def test_no_strict_guards_remains_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            3 <= { a(X) } <= 5.
            """, 
            """
            3 <= { a(X) } <= 5.
            """)
    
    def test_variable_in_guard_with_less_than_relation_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            Y <= { a(X) } < X.
            """, 
            """
            Y <= { a(X) } < X.
            """)

    def test_non_choice_head_is_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            p(X) :- q(X).
            """,
            """
            p(X) :- q(X).
            """
            )