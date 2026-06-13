"""
Unit tests for ``funasp.rewriting.restore`` paths that the integrated
pipeline cannot reach (head atoms are evaluable by construction there).
"""

import unittest

from funasp.core import ELibrary
from funasp.rewriting._context import RewriteContext
from funasp.rewriting.restore import restore_non_evaluable_functions
from funasp.util.ast import parse_string


class TestRestore(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = ELibrary()

    def test_restore_head(self):
        """A prefixed head whose signature is not evaluable is restored."""
        context = RewriteContext(self.lib, "F")
        statement = parse_string(self.lib, "a := 1 :- b.")[1]
        restored = restore_non_evaluable_functions(context, statement)
        self.assertEqual(str(restored), "a=1 :- b.")

    def test_restore_no_change(self):
        """A plain statement is left unchanged."""
        context = RewriteContext(self.lib, "F")
        statement = parse_string(self.lib, "p(X) :- q(X).")[1]
        restored = restore_non_evaluable_functions(context, statement)
        self.assertEqual(str(restored), "p(X) :- q(X).")
