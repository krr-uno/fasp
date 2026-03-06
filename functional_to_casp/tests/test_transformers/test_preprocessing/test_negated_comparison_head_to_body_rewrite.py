import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from casp.transformers.preprocessing.negated_comparison_head_to_body_rewrite import NegatedComparisonHeadToBodyTransformer
from casp.util.ast import AST

class ChoiceGuardTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = NegatedComparisonHeadToBodyTransformer(self.lib)

    def _apply(self, program: str) -> str:
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)
        rewritten: list[str] = []
        for node in nodes:
            new_node = self.transformer.rewrite_rule(node) or node
            rewritten.append(str(new_node).strip())
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

    def test_disjunction_head_with_comparison(self):
        program = """
        a | b<3 :- c.
        """
        expected = """
        a :- c; b>=3.
        """
        self.assertRewriteEqual(program, expected)

    def test_single_literal_head_with_comparison(self):
        program = """
        b<3 :- c.
        """
        expected = """
        :- c; b>=3.
        """
        self.assertRewriteEqual(program, expected)
    def test_rule_unchanged(self):
        program = """
        a :- c.
        """
        expected = """
        a :- c.
        """
        self.assertRewriteEqual(program, expected)    
    
    def test_negate_operator_all_cases(self):
        program = """
        a=1 :- d.
        a!=1 :- d.
        a<1 :- d.
        a<=1 :- d.
        a>1 :- d.
        a>=1 :- d.
        """
        expected = """
        :- d; a!=1.
        :- d; a=1.
        :- d; a>=1.
        :- d; a>1.
        :- d; a<=1.
        :- d; a<1.
        """
        self.assertRewriteEqual(program, expected)