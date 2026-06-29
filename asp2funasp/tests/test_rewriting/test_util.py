import unittest
from typing import Sequence

from clingo_funasp import ast
from clingo_funasp.core import Library

from asp2funasp.rewriting.util import (
    FreshFunctionNameGenerator,
    SurvivingSymbolSignatureCollector,
    build_function_name_index,
)
from asp2funasp.util.types import FRelation, SymbolSignature
from asp2funasp.util.util import index_frelations
from tests.util import collect_statements


class RewritingUtilTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    def _collect_signatures(
        self,
        program: str,
        frels: Sequence[FRelation],
        *,
        collect_single_node: bool = False,
    ) -> set[SymbolSignature]:
        nodes = collect_statements(self.lib, program)
        collector = SurvivingSymbolSignatureCollector(
            self.lib,
            index_frelations(frels),
        )

        if collect_single_node:
            return collector.collect(nodes[0])

        return collector.collect(nodes)

    def _make_non_function_symbolic_literal(self) -> ast.LiteralSymbolic:
        nodes = collect_statements(self.lib, "a.")
        rule = nodes[0]
        assert isinstance(rule, ast.StatementRule)

        variable_atom = ast.TermVariable(
            self.lib,
            rule.location,
            "X",
        )

        return ast.LiteralSymbolic(
            self.lib,
            rule.location,
            ast.Sign.NoSign,
            variable_atom,
        )

    def assertSignaturesEqual(
        self,
        actual: set[SymbolSignature],
        expected: set[SymbolSignature],
    ) -> None:
        self.assertEqual(
            actual,
            expected,
            msg=f"\nEXPECTED:\n{expected}\n\nACTUAL:\n{actual}",
        )

    def test_fresh_function_name_generator_skips_reserved_and_used_names(self):
        generator = FreshFunctionNameGenerator(
            reserved_names=[
                "assign",
                "assign_1",
            ],
        )

        self.assertEqual(generator.fresh("assign"), "assign_2")
        self.assertEqual(generator.fresh("assign"), "assign_3")
        self.assertEqual(generator.fresh("color"), "color_1")

    def test_build_function_name_index_renames_only_conflicting_inputs(self):
        frels = [
            FRelation(
                name="assign",
                arity=4,
                arguments=(0, 1, 2),
                values=[(3,)],
            ),
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
            FRelation(
                name="color",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            ),
        ]

        index = build_function_name_index(
            frels,
            conflicting_signatures={
                SymbolSignature("assign", 1),
                SymbolSignature("assign", 2),
            },
        )

        expected = {
            SymbolSignature("assign", 2): "assign_1",
            SymbolSignature("assign", 3): "assign_2",
            SymbolSignature("assign", 4): "assign",
            SymbolSignature("color", 2): "color",
        }

        self.assertEqual(index, expected)

    def test_collects_surviving_literal_and_nested_function_signatures(self):
        frels = [
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        program = """
        color(assign(N,C), red).
        assign(N,C,V).
        """

        expected = {
            SymbolSignature("color", 2),
            SymbolSignature("assign", 2),
        }

        self.assertSignaturesEqual(
            self._collect_signatures(program, frels),
            expected,
        )

    def test_collect_accepts_single_ast_node(self):
        frels: list[FRelation] = []

        program = """
        edge(node(a), X).
        """

        expected = {
            SymbolSignature("edge", 2),
            SymbolSignature("node", 1),
        }

        self.assertSignaturesEqual(
            self._collect_signatures(program, frels, collect_single_node=True),
            expected,
        )

    def test_rewritten_literal_does_not_collect_nested_function_signatures(self):
        frels = [
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        program = """
        assign(node(N), C, V).
        """

        self.assertSignaturesEqual(
            self._collect_signatures(program, frels),
            set(),
        )

    def test_collect_ignores_non_function_symbolic_literal(self):
        collector = SurvivingSymbolSignatureCollector(
            self.lib,
            frelation_index={},
        )

        self.assertSignaturesEqual(
            collector.collect(self._make_non_function_symbolic_literal()),
            set(),
        )

    def test_default_collector_visits_children_without_new_signatures(self):
        nodes = collect_statements(self.lib, ":- 1 = 1.")
        collector = SurvivingSymbolSignatureCollector(
            self.lib,
            frelation_index={},
        )

        self.assertSignaturesEqual(
            collector.collect(nodes),
            set(),
        )


# if __name__ == "__main__":
#     unittest.main()
