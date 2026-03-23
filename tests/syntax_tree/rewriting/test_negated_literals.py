import unittest

from clingo import ast

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.util.ast import ELibrary
from funasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negated_body_literals_from_statements,
)


class TestNegatedLiteralsTransformer(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = ELibrary()
        self.context = RewriteContext(self.lib)

    def apply_negated_literals_transformer(self, program: str):
        """
        Runs the negated literal transformer on the statement ASTs
        for a given string program

        :param program: The input ASP program str
        :type program: str
        """

        stmts = parse_string(self.lib, program)

        # skip #program directive if present
        stmts = (
            stmts[1:] if stmts and isinstance(stmts[0], ast.StatementProgram) else stmts
        )

        new_stmts = rewrite_negated_body_literals_from_statements(self.context, stmts)

        new_stmts_str = []
        for stmt in new_stmts:
            new_stmts_str.append(str(stmt).strip())

        return "\n".join(new_stmts_str).strip()

    def assertCorrectRewrite(self, program: str, expected_program: str):
        """Assert correct rewrite."""
        program = program.strip()
        new_program = self.apply_negated_literals_transformer(program)

        if expected_program is not None:
            expected_program = expected_program.strip()
            self.assertEqual(new_program, expected_program)

    def test_empty(self):
        """Test empty."""
        self.assertCorrectRewrite(
            "a.",
            "a.",
        )

    def test_no_change(self):
        """Test no change."""
        self.assertCorrectRewrite(
            "a :- b.",
            "a :- b.",
        )

    def test_basic(self):
        """Test basic."""
        self.assertCorrectRewrite(
            "b :- not a; c.",
            "b :- #false: a; c.",
        )

    def test_negated_literals_and_aggregates(self):
        """Test negated literals and aggregates."""
        self.assertCorrectRewrite(
            "d :- not a; #count { X: p(X), not b(X) } >= 2.",
            "d :- #false: a; #count { X: p(X), not b(X) } >= 2.",
        )

    def test_double_negation(self):
        """Test double negation."""
        self.assertCorrectRewrite(
            "b :- not not a.",
            "b :- not not a.",
        )

    def test_literal_boolean(self):
        """Test literal boolean."""
        self.assertCorrectRewrite(
            "b :- not #false.",
            "b :- not #false.",
        )

    def test_no_change_assignment(self):
        """Test no change assignment."""
        self.assertCorrectRewrite(
            "a := b.",
            "a := b.",
        )

    def test_negated_literals_and_aggregates_assignment(self):
        """Test negated literals and aggregates assignment."""
        self.assertCorrectRewrite(
            "score(X) := #sum{f(Y): f(p(Y)), q(X) } :- p; not a.",
            "score(X) := #sum{f(Y): f(p(Y)), q(X)} :- p; #false: a.",
        )

    def test_negated_literals_with_variables(self):
        """Test negated literals with variables."""
        self.assertCorrectRewrite(
            "a :- not p(X).",
            "a :- #false: p(X).",
        )

    def test_negated_literals_with_anonymous_variables(self):
        """Test negated literals with anonymous variables."""
        self.assertCorrectRewrite(
            "a :- not p(_).",
            "a :- #false: p(_).",
        )

    def test_orphan(self):
        """Test orphan."""
        self.assertCorrectRewrite(
            "orphan(X) :- person(X); not father(X)=_, not mother(X)=_.",
            "orphan(X) :- person(X); #false: father(X)=_; #false: mother(X)=_.",
        )
