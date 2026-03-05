import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from functional_to_casp.transformers.preprocessing.choice_rule_guard_normalize_rewrite import ChoiceGuardTransformer
from functional_to_casp.util.ast import AST

class ChoiceGuardTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = ChoiceGuardTransformer(self.lib)

    def _apply(self, program: str) -> str:
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)
        rewritten: list[str] = []
        for node in nodes:
            if isinstance(node, ast.StatementRule):
                new_node = self.transformer.rewrite_rule(node) or node
                rewritten.append(str(new_node).strip())
            else:
                rewritten.append(str(node).strip())
        # Remove the program declaration if present
        rewritten = rewritten[1:]
        return "\n".join(rewritten)

    def assertRewriteEqual(self, program: str, expected: str) -> None:
        result = self._apply(program)
        expected = textwrap.dedent(expected).strip()
        self.assertEqual(result, expected)

    # -----------------------------------------------------------
    # TESTS
    # -----------------------------------------------------------

    def test_no_guards_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            { p(X): q(X) } :- r(X).
            """,
            """
            { p(X): q(X) } :- r(X).
            """
        )

    def test_left_guard_normalization(self) -> None:
        self.assertRewriteEqual(
            """
            2 < { p(X): q(X) } :- r(X).
            """,
            """
            3 <= { p(X): q(X) } :- r(X).
            """
        )

    def test_right_guard_normalization(self) -> None:
        self.assertRewriteEqual(
            """
            { p(X): q(X) } < 5 :- r(X).
            """,
            """
            { p(X): q(X) } <= 4 :- r(X).
            """
        )

    def test_both_guards_normalization(self) -> None:
        self.assertRewriteEqual(
            """
            2 < { p(X): q(X) } < 5 :- r(X).
            """,
            """
            3 <= { p(X): q(X) } <= 4 :- r(X).
            """
        )

    def test_collapse_equal_bounds(self) -> None:
        self.assertRewriteEqual(
            """
            3 < { p(X): q(X) } < 5 :- r(X).
            """,
            """
            { p(X): q(X) } = 4 :- r(X).
            """
        )

if __name__ == "__main__":
    unittest.main()