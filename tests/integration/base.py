"""
Shared base class for the integration tests of the rewriting pipeline
(``funasp.ast._rewritings``) over the ``clingo_funasp`` parser.

Each test module under ``tests/integration/`` covers one type of construct
and asserts exact rewritten-program strings via ``assertTransformEqual``.
"""

import sys
import textwrap
import unittest

from clingo_funasp import ast

from funasp.ast import RewriteContext, parse_string, rewrite_statements
from funasp.core import Library
from funasp.util.types import SymbolSignature
from tests.restore_anonymous_term_variables import restore_anonymous_term_variables


class TransformTestCase(unittest.TestCase):
    """Base test case asserting exact rewritten-program strings."""

    def setUp(self):
        """Set up test fixtures for each test."""
        self.library = Library(logger=lambda t, m: print(m, file=sys.stderr))
        self.maxDiff = None  # Show full diff on assertion failure

    def assertTransformEqual(
        self,
        program: str,
        expected_program: str | None,
        *,
        intensional_functions: set[str] | None = None,
        prefix: str = "F",
        ignore_prefix_collisions: bool = False,
    ):
        """Assert that ``program`` rewrites exactly to ``expected_program``.

        Both texts are dedented and stripped; ``#program`` and comment
        statements are dropped from the comparison. ``intensional_functions``
        seeds extra signatures (as ``name/arity`` strings) into the context.
        """
        if intensional_functions is None:
            intensional_functions = set()

        intensional_functions = {
            SymbolSignature(name, int(arity))
            for name, arity in (s.split("/") for s in intensional_functions)
        }

        context = RewriteContext(
            self.library,
            prefix,
            intensional_functions=intensional_functions,
            ignore_prefix_collisions=ignore_prefix_collisions,
        )

        program = textwrap.dedent(program).strip()
        expected_program = (
            textwrap.dedent(expected_program).strip()
            if expected_program is not None
            else None
        )

        statement_asts = parse_string(self.library, program)
        transformed = [
            restore_anonymous_term_variables(context, statement)
            for wrapper in rewrite_statements(context, statement_asts)
            for statement in wrapper.rewritten
        ]

        transformed_str = "\n".join(
            str(statement).strip()
            for statement in transformed
            if not isinstance(statement, ast.StatementProgram | ast.StatementComment)
        )
        self.assertEqual(transformed_str, expected_program)
