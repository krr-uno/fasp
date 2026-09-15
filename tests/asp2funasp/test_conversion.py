import textwrap
import unittest
from unittest.mock import patch

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp import ConversionResult, RelationSkipReason, convert_statements
from funasp.asp2funasp.util.types import FRelation
from funasp.ast import RewriteContext, Statement, rewrite_statements
from funasp.core import Library as FunaspLibrary
from funasp.util.types import SymbolSignature
from tests.asp2funasp.util import collect_all_statements


class ConvertStatementsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.library = Library()

    def _convert(self, program: str, library: Library | None = None):
        active_library = library or self.library
        statements = collect_all_statements(
            active_library,
            textwrap.dedent(program).strip(),
        )
        return statements, convert_statements(active_library, statements)

    def assertEqualConversion(
        self,
        program: str,
        expected: str,
        library: Library | None = None,
    ) -> tuple[list[ast.Statement], ConversionResult]:
        """Convert ``program``, assert its complete output, and return metadata."""
        statements, result = self._convert(program, library)

        self.assertEqual(
            "\n".join(str(statement) for statement in result.converted_statements),
            textwrap.dedent(expected).strip(),
        )
        return statements, result

    def assertEqualPipeline(
        self,
        library: FunaspLibrary,
        program: str,
        expected_conversion: str,
        expected_rewrite: str,
        prefix: str = "G",
    ) -> ConversionResult:
        """Assert both ASP conversion and the downstream FUNASP rewrite."""
        _, result = self.assertEqualConversion(
            program,
            expected_conversion,
            library.library,
        )
        rewritten = rewrite_statements(
            RewriteContext(library, prefix_function=prefix),
            [
                Statement(library.library, statement)
                for statement in result.converted_statements
            ],
        )
        rewritten_program = "\n".join(
            str(statement) for wrapper in rewritten for statement in wrapper.rewritten
        )
        self.assertEqual(rewritten_program, textwrap.dedent(expected_rewrite).strip())
        return result

    def test_converts_detected_single_output_relation(self) -> None:
        statements, result = self.assertEqualConversion(
            "1 { assign(N,C) : color(C) } 1 :- node(N).",
            "1 <= { Fassign(N,C): color(C) } <= 1 :- node(N).",
        )

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

    def test_preprocessed_analysis_statements_are_not_emitted(self) -> None:
        statements, result = self._convert("{ p(X); q(X) } :- d(X).")

        self.assertEqual(len(statements), 1)
        self.assertEqual(result.converted_statements, tuple(statements))
        self.assertIs(result.converted_statements[0], statements[0])

    def test_emits_canonical_ast_for_configured_funasp_prefix(self) -> None:
        with FunaspLibrary() as library:
            self.assertEqualPipeline(
                library,
                """
                    1 { assign(N,C) : color(C) } 1 :- node(N).
                    selected(N,C) :- assign(N,C).
                """,
                """
                1 <= { Fassign(N,C): color(C) } <= 1 :- node(N).
                selected(N,C) :- assign(N)=C.
                """,
                """
                1 <= #count { 0,Gassign(N,C): Gassign(N,C): color(C) } <= 1 :- node(N).
                selected(N,C) :- Gassign(N,C).
                 :- Gassign(X0,_); 1 < #count { V: Gassign(X0,V) }.
                """,
            )

    def test_converts_matching_show_signature_end_to_end(self) -> None:
        with FunaspLibrary() as library:
            self.assertEqualPipeline(
                library,
                """
                    1 { assign(N,C) : color(C) } 1 :- node(N).
                    #show assign/2.
                    #show color/1.
                """,
                """
                1 <= { Fassign(N,C): color(C) } <= 1 :- node(N).
                #show Fassign/2. [true]
                #show color/1. [true]
                """,
                """
                1 <= #count { 0,Gassign(N,C): Gassign(N,C): color(C) } <= 1 :- node(N).
                #show Gassign/2. [true]
                #show color/1. [true]
                 :- Gassign(X0,_); 1 < #count { V: Gassign(X0,V) }.
                """,
            )

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
