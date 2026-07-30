import textwrap
import unittest
from unittest.mock import patch

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp import RelationSkipReason, convert_statements
from funasp.asp2funasp.util.types import FRelation
from funasp.ast import RewriteContext, Statement, rewrite_statements
from funasp.core import Library as FunaspLibrary
from funasp.util.types import SymbolSignature
from tests.asp2funasp.util import collect_statements


class ConvertStatementsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.library = Library()

    def _convert(self, program: str):
        statements = collect_statements(
            self.library,
            textwrap.dedent(program).strip(),
        )
        return statements, convert_statements(self.library, statements)

    def test_converts_detected_single_output_relation(self) -> None:
        statements, result = self._convert("1 { assign(N,C) : color(C) } 1 :- node(N).")

        self.assertEqual(len(result.functional_predicates), 1)
        self.assertEqual(
            result.accepted_relations,
            (FRelation("assign", 2, (0,), [(1,)]),),
        )
        self.assertEqual(result.skipped_relations, ())
        self.assertEqual(
            result.function_name_mapping,
            {SymbolSignature("assign", 2): "assign"},
        )
        self.assertEqual(len(result.converted_statements), len(statements))
        self.assertEqual(
            str(result.converted_statements[0]),
            "1 <= { Fassign(N,C): color(C) } <= 1 :- node(N).",
        )

    def test_preprocessed_analysis_statements_are_not_emitted(self) -> None:
        statements, result = self._convert("{ p(X); q(X) } :- d(X).")

        self.assertEqual(len(statements), 1)
        self.assertEqual(result.converted_statements, tuple(statements))
        self.assertIs(result.converted_statements[0], statements[0])

    def test_emits_canonical_ast_for_configured_funasp_prefix(self) -> None:
        with FunaspLibrary() as library:
            statements = collect_statements(
                library.library,
                textwrap.dedent("""
                    1 { assign(N,C) : color(C) } 1 :- node(N).
                    selected(N,C) :- assign(N,C).
                    """).strip(),
            )
            result = convert_statements(library.library, statements)

            self.assertEqual(
                str(result.converted_statements[0]),
                "1 <= { Fassign(N,C): color(C) } <= 1 :- node(N).",
            )
            self.assertEqual(
                str(result.converted_statements[1]),
                "selected(N,C) :- assign(N)=C.",
            )

            context = RewriteContext(library, prefix_function="G")
            rewritten = rewrite_statements(
                context,
                [
                    Statement(library.library, statement)
                    for statement in result.converted_statements
                ],
            )
            rewritten_program = "\n".join(
                str(statement)
                for wrapper in rewritten
                for statement in wrapper.rewritten
            )

            self.assertIn("selected(N,C) :- Gassign(N,C).", rewritten_program)
            self.assertNotIn("Fassign", rewritten_program)
            self.assertNotIn("assign(N)=C", rewritten_program)

    def test_skips_multiple_output_positions(self) -> None:
        statements, result = self._convert("""
            :- pos(I,X,Y); pos(I,X1,Y1); X1 != X.
            :- pos(I,X,Y1); pos(I,X1,Y); Y1 != Y.
            """)

        self.assertEqual(len(result.functional_predicates), 2)
        self.assertEqual(result.accepted_relations, ())
        self.assertEqual(result.converted_statements, tuple(statements))
        self.assertEqual(result.function_name_mapping, {})
        self.assertEqual(len(result.skipped_relations), 1)
        self.assertEqual(
            result.skipped_relations[0].relation,
            FRelation("pos", 3, (0,), [(1,), (2,)]),
        )
        self.assertEqual(
            result.skipped_relations[0].reason,
            RelationSkipReason.UNSUPPORTED_OUTPUT_COUNT,
        )

    def test_records_relation_skipped_for_head_disjunction(self) -> None:
        with patch(
            "funasp.asp2funasp.conversion.remove_frelations_in_head_disjunctions",
            return_value=[],
        ):
            statements, result = self._convert(
                "1 { assign(N,C) : color(C) } 1 :- node(N)."
            )

        self.assertEqual(result.accepted_relations, ())
        self.assertEqual(result.converted_statements, tuple(statements))
        self.assertEqual(result.function_name_mapping, {})
        self.assertEqual(len(result.skipped_relations), 1)
        self.assertEqual(
            result.skipped_relations[0].reason,
            RelationSkipReason.HEAD_DISJUNCTION,
        )


if __name__ == "__main__":
    unittest.main()
