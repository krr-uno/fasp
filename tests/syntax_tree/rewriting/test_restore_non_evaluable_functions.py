import unittest
from typing import List, Set

from clingo import ast

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import AssignmentRule
from funasp.syntax_tree.rewritings.restore_non_evaluable_functions import (
    _restore_literal,
    restore_non_evaluable_functions_list,
)
from funasp.syntax_tree.rewritings.to_asp import to_asp
from funasp.syntax_tree.types import SymbolSignature
from funasp.syntax_tree import FASP_Statement
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.util.ast import ELibrary


class TestRestoreNonEvaluableFunctions(unittest.TestCase):
    """Tests for restoring non-evaluable prefixed function literals."""

    def setUp(self) -> None:
        self.elib = ELibrary()

    @staticmethod
    def _clingo_rewrite_wrapper(
        context: RewriteContext,
        statements: List[ast.Statement],
    ) -> List[ast.Statement]:
        ctx = context.ctx
        context.lib.ignore_info = True
        out: List[ast.Statement] = []
        errors = []
        for stmt in statements:
            try:
                assert not isinstance(stmt, AssignmentRule)
                rewritten_list = ast.rewrite_statement(ctx, stmt)
            except RuntimeError as e:
                errors.append((stmt, e))
                continue
            out.extend(rewritten_list)
        context.lib.ignore_info = False
        if errors:
            raise RuntimeError("rewriting failed", errors)
        return out

    def assertEqualRestore(
        self,
        evaluable_functions: Set[SymbolSignature],
        program: str,
        expected: str,
        *,
        prefix: str = "pf_",
        use_to_asp: bool = False,
    ) -> None:
        """Parse -> optional to_asp -> clingo rewrite -> restore, then compare text."""
        context = RewriteContext(
            self.elib,
            prefix_function=prefix,
            evaluable_functions=evaluable_functions,
        )
        statements: List[FASP_Statement] = parse_string(self.elib, program)

        if use_to_asp:
            statements = [to_asp(context, stmt) for stmt in statements]

        rewritten = self._clingo_rewrite_wrapper(context, statements)
        restored = restore_non_evaluable_functions_list(context, rewritten)

        restored = [
            stmt for stmt in restored if not isinstance(stmt, ast.StatementProgram)
        ]
        restored_str = "\n".join(str(stmt).strip() for stmt in restored)
        self.assertEqual(restored_str, expected)

    def test_restore_non_evaluable_prefixed_literal(self) -> None:
        program = "p :- pf_f(a,b,c)."
        expected = "p :- f(a,b)=c."
        self.assertEqualRestore(set(), program, expected)

    def test_integration_to_asp_keeps_evaluable(self) -> None:
        program = "p :- f(a,b)=c."
        expected = "p :- pf_f(a,b,c)."
        self.assertEqualRestore(
            {SymbolSignature("f", 2)},
            program,
            expected,
            use_to_asp=True,
        )

    def test_literalsymbolic_without_term_function_or_term_symbolic_returns_none(self) -> None:
        """A LiteralSymbolic that doesn't wrap a TermFunction or TermSymbolic is not restorable."""
        program = "p."
        self.assertEqualRestore(set(), program, program)

    # def test_restore_literal_prefixed_zero_arity_returns_none(self) -> None:
    #     """A prefixed zero-arity literal has no value slot and is not restorable."""
    #     context = RewriteContext(
    #         self.elib,
    #         prefix_function="pf_",
    #         evaluable_functions=set(),
    #     )
    #     statements = parse_string(self.elib, "p :- pf_f.")
    #     assert len(statements) >= 2
    #     assert isinstance(statements[1], ast.StatementRule)
    #     body_literal = statements[1].body[0]
    #     assert isinstance(body_literal, ast.LiteralSymbolic)
    #     self.assertIsNone(_restore_literal(context, body_literal))
    


## EXTRA TESTS ##

    # def test_keep_evaluable_prefixed_literal(self) -> None:
    #     program = "p :- pf_f(a,b,c)."
    #     expected = "p :- pf_f(a,b,c)."
    #     self.assertEqualRestore({SymbolSignature("f", 2)}, program, expected)

    # def test_restore_after_clingo_unpool(self) -> None:
    #     program = "p :- pf_f((1;a,c),c): a,b,c."
    #     expected = "p :- f(1)=c: a, b, c; f((a,c))=c: a, b, c."
    #     self.assertEqualRestore(set(), program, expected)

    # def test_keep_symbolic_atom_unchanged(self) -> None:
    #     program = "p :- foo."
    #     expected = "p :- foo."
    #     self.assertEqualRestore(set(), program, expected)

    # def test_head_literal_unchanged(self) -> None:
    #     program = "pf_f(a,b,c) :- d."
    #     expected = "f(a,b)=c :- d."
    #     self.assertEqualRestore(set(), program, expected)
if __name__ == "__main__":
    unittest.main()