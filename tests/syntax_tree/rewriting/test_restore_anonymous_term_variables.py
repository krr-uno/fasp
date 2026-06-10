import unittest
import textwrap
from typing import List

from clingo import ast

from funasp.fun_ast._context import RewriteContext
from tests.restore_anonymous_term_variables import (
    restore_anonymous_term_variables
)

from funasp.fun_ast.rewritings.to_asp import to_asp
from funasp.fun_ast.parsing.parser import parse_string
from funasp.fun_ast.types import SymbolSignature
from funasp.fun_ast._nodes import AssignmentRule
from funasp.fun_ast import FASP_Statement
from funasp.util.ast import ELibrary


class TestRestoreNonEvaluableFunctions(unittest.TestCase):
    """Tests for restoring anonymous term variables."""

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
        prefix:str = "pf_",
        rewrite:bool = True,
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
        if rewrite:
            # Clingo rewrite to normalize AST
            statements = self._clingo_rewrite_wrapper(context, statements)

        # Restore anonymous term variables
        statements = [restore_anonymous_term_variables(context, stmt) for stmt in statements]

        # Skip the #program directive if present
        if statements:
            statements = statements[1:]

        restored_strs = [str(stmt).strip() for stmt in statements]
        str_restored = '\n'.join(restored_strs) if restored_strs else ""

        self.assertEqual(
            str_restored, textwrap.dedent(expected).strip()
        )

    ## TESTS ##

    def test_restore(self) -> None:
        """Test restore anonynomus after clingo rewrite."""
        program = """
                father(cain):=adam.
                father(able):=adam.
                person(Y) :- father(_)=Y.
                """
        expected = """
                pf_father(cain,adam).
                pf_father(able,adam).
                person(Y) :- pf_father(_,Y).
                """
        self.assertEqualRestore({SymbolSignature("father", 1)}, program, expected)

    def test_no_restore(self) -> None:
        """Test no restore anonynomus if '_'."""
        program = """
                father(cain):=adam.
                father(able):=adam.
                person(Y) :- father(_)=Y.
                """
        expected = """
                pf_father(cain,adam).
                pf_father(able,adam).
                person(Y) :- pf_father(_,Y).
                """
        self.assertEqualRestore({SymbolSignature("father", 1)}, program, expected, rewrite=False)

    ## EXTRA TESTS ##


if __name__ == "__main__":
    unittest.main()
