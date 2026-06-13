"""
Unit tests for ``funasp.rewriting.some_assignments.rewrite_some_assignments``.

The function rewrites the parser's ``FS``-marked ``#some`` head aggregate into
a choice with a count guard, producing one statement per pool entry on the
left guard.
"""

import unittest

from funasp.rewriting._context import RewriteContext
from funasp.rewriting.some_assignments import rewrite_some_assignments
from funasp.ast import parse_string
from funasp.core import Library


class TestRewriteSomeAssignments(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def rewrite(self, code: str) -> list[str]:
        """Parse a program and rewrite each statement (skipping the base directive)."""
        context = RewriteContext(self.lib, "F")
        statements = parse_string(self.lib, code)
        result: list[str] = []
        for statement in statements[1:]:
            result.extend(str(s) for s in rewrite_some_assignments(context, statement))
        return result

    def test_single_pool_returns_one_statement(self):
        """An unpooled left term yields a single choice statement."""
        self.assertEqual(
            self.rewrite("f(a) := #some{X : p(X)}."),
            ["{ Ff(a,X): p(X) } = 1 :- #count { X: p(X) } >= 1."],
        )

    def test_multiple_pools_return_one_statement_each(self):
        """A pooled left term yields one choice statement per pool entry."""
        self.assertEqual(
            self.rewrite("f(a;b) := #some{X : p(X)}."),
            [
                "{ Ff(a,X): p(X) } = 1 :- #count { X: p(X) } >= 1.",
                "{ Ff(b,X): p(X) } = 1 :- #count { X: p(X) } >= 1.",
            ],
        )

    def test_non_some_statement_passes_through(self):
        """A statement that is not a ``#some`` assignment is returned unchanged."""
        self.assertEqual(self.rewrite("p(X) :- q(X)."), ["p(X) :- q(X)."])
