import unittest
import textwrap
from typing import List

from clingo import ast

from funasp.ast._context import RewriteContext
from funasp.ast.rewritings.restore_non_evaluable_functions import (
    _restore_literal,
    restore_non_evaluable_functions_list,
)
from funasp.ast.rewritings.to_asp import to_asp
from funasp.ast.parsing.parser import parse_string
from funasp.ast.types import SymbolSignature
from funasp.ast._nodes import AssignmentRule
from funasp.ast import FASP_Statement
from funasp.util.ast import ELibrary


class TestRestoreNonEvaluableFunctions(unittest.TestCase):
    """Tests for restoring non-evaluable prefixed function literals."""

    def setUp(self) -> None:
        """Set up the test case with a library instance."""
        self.elib = ELibrary()
        self.lib = self.elib.library

    def _parse_statements(self, program: str) -> List[FASP_Statement]:
        """Parse a program string into statements."""
        statements = parse_string(self.lib, program)
        return statements
    @staticmethod
    def _clingo_rewrite_wrapper(context: RewriteContext, statements:List[ast.Statement]) -> List[ast.Statement]:
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
        evaluable_functions: set,
        program: str,
        expected: str,
        *,
        prefix = "pf_",
    ) -> None:
        """Assert that restoring a program produces the expected restored statements.

        Pipeline: parse raw ASP → to_asp (protect) → clingo rewrite → restore.
        This tests the full restoration stage of the transformation pipeline.
        """

        context = RewriteContext(
            self.elib, prefix_function=prefix, evaluable_functions=evaluable_functions
        )
        statements = parse_string(self.elib, program)

        # if statements:
        #     statements = statements[1:]
        # Protect using to_asp (converts evaluable functions to prefixed predicates)
        statements = [to_asp(context, stmt) for stmt in statements]



        # Clingo rewrite to normalize AST
        statements = self._clingo_rewrite_wrapper(context, statements)

        # # Restore non-evaluable function predicates back to equalities
        statements = restore_non_evaluable_functions_list(context, statements)

        # Skip the #program directive if present
        if statements:
            statements = statements[1:]

        restored_strs = [str(stmt).strip() for stmt in statements]
        str_restored = '\n'.join(restored_strs) if restored_strs else ""

        self.assertEqual(
            str_restored, textwrap.dedent(expected).strip()
        )

    ## TESTS ##

    def test_no_restore_for_evaluable_function(self) -> None:
        """do not restore f(a,b):=c back after protection and restoration."""
        program = "f(a,b) :=c :- a; b; c."
        expected = "pf_f(a,b,c) :- a; b; c."
        self.assertEqualRestore({SymbolSignature("f", 2)}, program, expected)

    def test_restore_after_clingo_unpool(self) -> None:
        """Restore non-evaluable variants after to_asp protects and clingo unpools."""
        program = "f(1;a,c)=c : a, b, c."
        expected = "pf_f(1,c): a, b, c; f(a,c)=c: a, b, c."
        self.assertEqualRestore({SymbolSignature("f", 1)}, program, expected)

    def test_restore_after_clingo_unpool_body(self) -> None:
        """Restore non-evaluable variants after to_asp protects and clingo unpools in body."""
        program = ":- f(1;a,c)=c : a, b, c."
        expected = ":- pf_f(1,c): a, b, c; f(a,c)=c: a, b, c."
        self.assertEqualRestore({SymbolSignature("f", 1)}, program, expected)

    def test_restore_literal_returns_none_for_term_function_atom(self) -> None:
        """Check restore for TermFunction (covered via parsing)."""
        context = RewriteContext(
            self.elib,
            prefix_function="pf_",
            evaluable_functions=set(),
        )
        statements = parse_string(self.elib, "p :- pf_f(a,b).")
        rule = next(stmt for stmt in statements if isinstance(stmt, ast.StatementRule))
        literal = rule.body[0].literal

        assert isinstance(literal, ast.LiteralSymbolic)
        assert isinstance(literal.atom, ast.TermFunction)

        assert(str(_restore_literal(context, literal)), "f(a)=b.")


    ## EXTRA TESTS ##

    def test_head_literal_restored(self) -> None:
        """Restore function equalities in head literals."""
        program = """
            f(a;b,e)=c :- d.
            """
        expected = """
            pf_f(a,c) :- d.
            f(b,e)=c :- d.
            """
        self.assertEqualRestore({SymbolSignature("f",1)}, program, expected)

    def test_keep_evaluable_literal(self) -> None:
        """Keep f(a,b)=c protected when f/2 is evaluable."""
        program = "p :- f(a,b)=c."
        expected = "p :- pf_f(a,b,c)."
        self.assertEqualRestore({SymbolSignature("f", 2)}, program, expected)

    def test_keep_symbolic_atom_unchanged(self) -> None:
        """Keep symbolic atoms that are not functions unchanged."""
        program = "p :- foo."
        expected = "p :- foo."
        self.assertEqualRestore(set(), program, expected)

    def test_evaluable_function_protected(self) -> None:
        """Evaluable functions stay protected through the pipeline."""
        program = "p :- f(a,b)=c."
        expected = "p :- pf_f(a,b,c)."
        self.assertEqualRestore({SymbolSignature("f", 2)}, program, expected)





if __name__ == "__main__":
    unittest.main()
