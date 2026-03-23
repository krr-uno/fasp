import textwrap
import unittest

from clingo import ast
from funasp.util.ast import ELibrary
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.syntax_tree import rewrite_statement, rewrite_statements
from funasp.syntax_tree._context import RewriteContext


class TestSyntaxTree(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.elib = ELibrary()
        self.ctx = RewriteContext(self.elib)

    def applyRewrite(self, program: str):
        """Apply rewrite."""
        statement_asts = parse_string(self.elib, program)[1:]
        rewritten_statements: list[ast.Statement] = []
        for stmt in statement_asts:
            rewritten_statements.extend(rewrite_statement(self.ctx, stmt))

        return rewritten_statements

    def applyRewriteList(self, program: str):
        """Apply rewritelist."""
        statement_asts = parse_string(self.elib, program)[1:]
        rewritten_statements = rewrite_statements(self.ctx, statement_asts)
        return rewritten_statements

    def assertRewriteEqual(self, program: str, expected: str, mode: int = 1):
        """Assert rewrite equal."""
        if mode == 1:
            rewritten_statements = self.applyRewrite(program)
        else:
            rewritten_statements = self.applyRewriteList(program)
        rewritten_program_str = "\n".join(
            str(stmt).strip() for stmt in rewritten_statements
        )
        self.assertEqual(
            rewritten_program_str.strip(),
            textwrap.dedent(expected).strip(),
        )

    def test_rewrite_statement(self):
        """Test rewrite statement."""
        program = "f(1) := Y :- g(Y)."
        expected = """\
            Ff(1,Y) :- g(Y).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """
        self.assertRewriteEqual(program, expected, mode=1)

    def test_rewrite_statements(self):
        """Test rewrite statements."""
        program = "f(1) := Y :- g(Y)."
        expected = """\
            Ff(1,Y) :- g(Y).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """
        self.assertRewriteEqual(program, expected, mode=2)
