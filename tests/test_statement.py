"""
Unit tests for the ``Statement`` wrapper in ``funasp.ast._core``.
"""

import unittest

from clingo_funasp import ast

from funasp.ast import parse_string
from funasp.core import Library


class TestStatementRewrite(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def test_expanding_rewrite(self):
        """A list-returning rewrite function replaces the statement in place."""
        statement = parse_string(self.lib, "a.")[1]

        statement.rewrite(lambda stmt: [stmt, stmt])

        self.assertEqual(len(statement.rewritten), 2)

    def test_failed_rewrite_leaves_statement_unchanged(self):
        """A rewrite function that raises leaves the rewritten list untouched.

        Regression test: the rewritten list used to be reassigned inside the
        rewrite loop, so a mid-loop exception left it half-migrated.
        """
        statement = parse_string(self.lib, "a.")[1]
        statement.rewrite(lambda stmt: [stmt, stmt])
        seen: list[ast.Statement] = []

        def failing(stmt: ast.Statement) -> ast.Statement:
            if seen:
                raise RuntimeError("rewrite failed mid-loop")
            seen.append(stmt)
            return stmt

        with self.assertRaisesRegex(RuntimeError, "rewrite failed mid-loop"):
            statement.rewrite(failing)

        self.assertEqual(len(statement.rewritten), 2)


if __name__ == "__main__":
    unittest.main()
