import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from functional_to_casp.transformers.preprocessing.notaggregate_constraint_rewrite import NotAggregateConstraintTransformer
from functional_to_casp.util.ast import AST

class NotAggregateConstraintTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = NotAggregateConstraintTransformer(self.lib)

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

    def test_non_constraint_rule_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            p(X) :- not 1 <= #count { X: a(X) }.
            """,
            """
            p(X) :- not 1 <= #count { X: a(X) }.
            """
        )

    def test_constraint_without_negated_aggregate_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            :- 1 <= #count { X: a(X) }.
            """,
            """
            :- 1 <= #count { X: a(X) }.
            """
        )

    def test_negated_body_aggregate_constraint(self) -> None:
        self.assertRewriteEqual(
            """
            :- not 1 <= #count { X: a(X) }.
            """,
            """
            :- 1 > #count { X: a(X) }.
            """
        )

    def test_multiple_body_literals(self) -> None:
        self.assertRewriteEqual(
            """
            :- not 1 <= #count { X: a(X) }; b(X).
            """,
            """
            :- 1 > #count { X: a(X) }; b(X).
            """
        )

if __name__ == "__main__":
    unittest.main()
