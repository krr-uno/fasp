"""
Unit tests for ``funasp.ast._rewritings.restore`` paths that the integrated
pipeline cannot reach (head atoms are intensional by construction there).
"""

import unittest

from funasp.astt import RewriteContext, parse_string
from funasp.astt._rewritings.restore import restore_non_intensional_functions
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
