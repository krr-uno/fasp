import unittest

from clingo import ast

from fasp.syntax_tree.parsing.parser import parse_string
from fasp.util.ast import ELibrary
from fasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negated_body_literals_from_statements,
)


class TestNegatedLiteralsTransformer(unittest.TestCase):
    def setUp(self):
        self.lib = ELibrary()

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

        new_stmts = rewrite_negated_body_literals_from_statements(self.lib, stmts)

        new_stmts_str = []
        for stmt in new_stmts:
            new_stmts_str.append(str(stmt).strip())

        return "\n".join(new_stmts_str).strip()

    def assertCorrectRewrite(self, program: str, expected_program: str):
        program = program.strip()
        new_program = self.apply_negated_literals_transformer(program)

        if expected_program is not None:
            expected_program = expected_program.strip()
            self.assertEqual(new_program, expected_program)

    def test_empty(self):
        self.assertCorrectRewrite(
            "a.",
            "a.",
            )

    def test_no_change(self):
        self.assertCorrectRewrite(
            "a :- b.",
            "a :- b.",
            )

    def test_basic(self):
        self.assertCorrectRewrite(
            "b :- not a; c.",
            "b :- #false: a; c.",
            )

    def test_negated_literals_and_aggregates(self):
        self.assertCorrectRewrite(
            "d :- not a; #count { X: p(X), not b(X) } >= 2.",
            "d :- #false: a; #count { X: p(X), not b(X) } >= 2.",
        )

    def test_double_negation(self):
        self.assertCorrectRewrite(
            "b :- not not a.",
            "b :- not not a.",
            )

    def test_literal_boolean(self):
        self.assertCorrectRewrite(
            "b :- not #false.",
            "b :- not #false.",
            )

    def test_no_change_assignment(self):
        self.assertCorrectRewrite(
            "a := b.",
            "a := b.",
            )

    def test_negated_literals_and_aggregates_assignment(self):
        self.assertCorrectRewrite(
            "score(X) := #sum{f(Y): f(p(Y)), q(X) } :- p; not a.",
            "score(X) := #sum{f(Y): f(p(Y)), q(X)} :- p; #false: a.",
        )

    def test_negated_literals_with_variables(self):
        self.assertCorrectRewrite(
            "a :- not p(X).",
            "a :- #false: p(X).",
        )

    def test_negated_literals_with_anonymous_variables(self):
        self.assertCorrectRewrite(
            "a :- not p(_).",
            "a :- #false: p(_).",
        )

    def test_orphan(self):
        self.assertCorrectRewrite(
            "orphan(X) :- person(X); not father(X)=_, not mother(X)=_.",
            "orphan(X) :- person(X); #false: father(X)=_; #false: mother(X)=_.",
        )
