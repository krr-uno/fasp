


import unittest

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.util.ast import ELibrary
from funasp.syntax_tree.rewritings.some_assignments import (
    _rewrite_some_choices,
)


class TestChoiceSomeToChoiceAssignment(unittest.TestCase):
    """Unit tests for the ChoiceSome -> ChoiceAssignment transformer."""

    def setUp(self):
        self.lib = ELibrary()
        self.context = RewriteContext(self.lib)

    def parse_program(self, program: str):
        """Parse a FASP program into statements (AssignmentRule)."""
        return parse_string(self.lib, program)

    def rewrite(self, program: str):
        """Apply the transformer to all rules in the program."""
        stmts = self.parse_program(program)
        stmts = stmts[1:]
        out: list[str] = []
        for stmt in stmts:
            transformed = _rewrite_some_choices(
                self.context, stmt
            )
            stmt = transformed or stmt
            out.append(str(stmt).strip())
        return out, []

    def assertRewriteEqual(self, program: str, expected: str):
        result, errors = self.rewrite(program)
        expected_lines = [line.strip() for line in expected.splitlines()]
        self.assertCountEqual(result, expected_lines)
        self.assertEqual(errors, [])

    def test_choice_some_transformation(self):
        self.assertRewriteEqual(
            "a := #some{X: p(X)} :- p.",
            "{ a := X: p(X) } = 1 :- #count { X: p(X) } >= 1; p.",
        )

    def test_choice_some_transformation_2(self):
        self.assertRewriteEqual(
            "a := #some{X: p(X), q; Y: p(Y)} :- p.",
            "{ a := X: p(X), q; a := Y: p(Y) } = 1 :- #count { X: p(X), q; Y: p(Y) } >= 1; p.",
        )

    def test_non_choice_some_transformation(self):
        self.assertRewriteEqual(
            "a := #sum{X: p(X)} :- p.",
            "a := #sum{X: p(X)} :- p.",
        )

    def test_non_assignment_transformation(self):
        self.assertRewriteEqual(
            "#sum{ X: p(X) } :- p.",
            "#sum { X: p(X) } :- p.",
        )

    def test_choice_some_tuple(self):
        self.assertRewriteEqual(
            "a := #some{X,Y: p(X,Y)} :- p.",
            "{ a := (X,Y): p(X,Y) } = 1 :- #count { X,Y: p(X,Y) } >= 1; p.",
        )
