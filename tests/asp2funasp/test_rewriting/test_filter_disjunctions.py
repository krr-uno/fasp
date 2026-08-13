import re
import textwrap
import unittest
from typing import List

from clingo_funasp import ast
from clingo_funasp.core import Library
from clingo_funasp.symbol import parse_term

from funasp.asp2funasp.rewriting.filter_disjunctions import (
    HeadDisjunctionFRelationCollector,
    remove_frelations_in_head_disjunctions,
)
from funasp.asp2funasp.rewriting.rewrite_into_funasp import (
    FunctionalPredicateRewriteTransformer,
)
from funasp.asp2funasp.util.types import FRelation
from funasp.asp2funasp.util.util import index_frelations
from funasp.util.ast import AST
from tests.asp2funasp.util import collect_statements


class FunctionalPredicateRewriteTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    def _normalize(self, s: str) -> str:
        return re.sub(r"\s+", "", s)

    # APPLY TRANSFORMER
    def _rewrite(
        self,
        program: str,
        frels: List[FRelation],
    ) -> List[AST]:
        program = textwrap.dedent(program).strip()

        nodes: List[AST] = collect_statements(self.lib, program)

        safe_frelations = remove_frelations_in_head_disjunctions(
            self.lib,
            nodes,
            frels,
        )
        transformer = FunctionalPredicateRewriteTransformer.from_program(
            self.lib,
            safe_frelations,
            nodes,
        )

        new_nodes: List[AST] = []

        for node in nodes:
            new_node = transformer.transform_statement(node)

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
        
        # Compare via string form (standard in clingo AST tests)
        actual_str = self._normalize("\n".join(str(n) for n in actual_nodes))
        # expected_str = "\n".join(str(n) for n in expected_nodes)
        expected_str = self._normalize(textwrap.dedent(expected).strip())

        self.assertEqual(
            actual_str,
            expected_str,
            msg=f"\nEXPECTED:\n{expected_str}\n\nACTUAL:\n{actual_str}",
        )

    def _make_non_function_symbolic_literal(self) -> ast.LiteralSymbolic:
        location = collect_statements(self.lib, "a.")[0].location

        return ast.LiteralSymbolic(
            self.lib,
            location,
            ast.Sign.NoSign,
            ast.TermSymbolic(
                self.lib,
                location,
                parse_term(self.lib, "42"),
            ),
        )

    def test_removes_frelation_in_head_disjunction(self):

        frels = [
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        program = """
        assign(N,C,V) | other(N) :- node(N).
        assign(N,C,V) :- ok(N,C,V).
        """

        expected = program = """
        assign(N,C,V) ; other(N) :- node(N).
        assign(N,C,V) :- ok(N,C,V).
        """

        self.assertEqualRewrite(
            program,
            program,frels)

    def test_generic_collector_visits_non_rule_ast_node(self):

        frels = [
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        program = """
        assign(N,C,V) | other(N) :- node(N).
        """

        nodes = collect_statements(self.lib, textwrap.dedent(program).strip())
        collector = HeadDisjunctionFRelationCollector(
            self.lib,
            index_frelations(frels),
        )

        collector._collect(nodes[0].head)

        self.assertEqual(collector.blocked_signatures, set())

    def test_non_function_symbolic_literal_is_not_blocked(self):

        frels = [
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        collector = HeadDisjunctionFRelationCollector(
            self.lib,
            index_frelations(frels),
        )

        collector._block_literal_if_frelation(
            self._make_non_function_symbolic_literal(),
        )

        self.assertEqual(collector.blocked_signatures, set())
