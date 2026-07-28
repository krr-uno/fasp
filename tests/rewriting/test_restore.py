"""
Unit tests for ``funasp.ast._rewritings.restore`` paths that the integrated
pipeline cannot reach (head atoms are intensional by construction there).
"""

import unittest

from clingo_funasp import ast
from clingo_funasp.symbol import Number

from funasp.ast import RewriteContext, parse_string
from funasp.ast._rewritings.restore import restore_non_intensional_functions
from funasp.core import Library


class TestRestore(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def test_restore_head(self):
        """A prefixed head whose signature is not intensional is restored."""
        context = RewriteContext(self.lib, "F")
        statement = parse_string(self.lib, "a := 1 :- b.")[1].original
        restored = restore_non_intensional_functions(context, statement)
        self.assertEqual(str(restored), "a=1 :- b.")

    def test_restore_no_change(self):
        """A plain statement is left unchanged."""
        context = RewriteContext(self.lib, "F")
        statement = parse_string(self.lib, "p(X) :- q(X).")[1].original
        restored = restore_non_intensional_functions(context, statement)
        self.assertEqual(str(restored), "p(X) :- q(X).")

    def test_non_function_atom_left_unchanged(self):
        """A symbolic literal whose atom is not a function is left untouched."""
        context = RewriteContext(self.lib, "F")
        library = self.lib.library
        statement = parse_string(self.lib, "a :- b.")[1].original
        number_atom = ast.TermSymbolic(library, statement.location, Number(library, 1))
        literal = ast.LiteralSymbolic(
            library, statement.location, ast.Sign.NoSign, number_atom
        )
        statement = statement.update(
            library, body=[ast.BodySimpleLiteral(library, literal)]
        )

        restored = restore_non_intensional_functions(context, statement)

        self.assertIs(restored, statement)
