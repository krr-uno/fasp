import textwrap
import unittest

from clingo.core import Library

from fasp.ast.rewriting_aggregates import (
    normalize_assignment_aggregates,
)

from fasp.ast.tree_sitter.parser import parse_string

class TestHeadAggregateToBodyRewriteTransformer(unittest.TestCase):
    """
    Unit tests for the HeadAggregateToBodyRewriteTransformer.
    """

    def setUp(self):
        self.lib = Library()

    def parse_program(self, program: str):
        stmts = parse_string(self.lib, program)
        # ast.parse_string(self.lib, program, stmts.append)
        return stmts

    def rewrite(self, program: str):
        stmts = self.parse_program(program)
        out = [normalize_assignment_aggregates(self.lib, stmt) for stmt in stmts]
        return [str(stmt).strip() for stmt in out], []

    def assertRewriteEqual(self, program: str, expected: str):
        result, errors = self.rewrite(program)
        expected_lines = [line.strip() for line in expected.splitlines()]
        self.assertCountEqual(result, expected_lines)
        # also check that no errors were collected
        self.assertEqual(errors, [])

    def test_valid_sum_and_count(self):
        program = """\
            f(X) := #sum { Y: p(Y,Z) , q(X), r(X) } :- b(X,Z).
            f(X) := #count { Y: p(Y,Z) } :- b(X,Z).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            f(X) := W :- b(X,Z); W = #sum { Y: p(Y,Z), q(X), r(X) }.
            f(X) := W :- b(X,Z); W = #count { Y: p(Y,Z) }.
        """
        ).strip()
        self.assertRewriteEqual(program, expected)

    def test_valid_sum_and_count2(self):
        program = """\
            f(X) := #sum{ Y : p(Y,Z) , q(X), r(X) ; X : p(X) , q(X), r(X) } :- b(X,Z).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            f(X) := W :- b(X,Z); W = #sum { Y: p(Y,Z), q(X), r(X); X: p(X), q(X), r(X) }.
        """
        ).strip()
        self.assertRewriteEqual(program, expected)


    def test_valid_left_term_no_error_symbolic_function(self):
        program = """\
            a = #sum{ Y : p(Y,Z) } :- b(X,Z).
        """
        _, errors = self.rewrite(program)
        self.assertEqual(len(errors), 0)
        # This is correct
        # a is TermSymbolic whose symbol is a function. Numbers or string are not valid here.

    # def test_invalid_left_term_error_string(self):
    #     program = """\
    #         "as" := #sum{ Y : p(Y,Z) } :- b(X,Z).
    #     """
    #     _, errors = self.rewrite(program)

    #     self.assertEqual(len(errors), 1)
    #     self.assertIn(
    #         "The left-hand side of an assignment must be a function term",
    #         errors[0].message,
    #     )
        # 'The left-hand side of an assignment must be a function term, found "as"'

    # def test_invalid_left_term_error_number(self):
    #     program = """\
    #         1 = #sum{ Y : p(Y,Z) } :- b(X,Z).
    #     """
    #     _, errors = self.rewrite(program)

    #     self.assertEqual(len(errors), 1)
    #     self.assertIn(
    #         "The left-hand side of an assignment must be a function term",
    #         errors[0].message,
    #     )
    #     self.assertIn("found 1", errors[0].message)

    # # Note: Does aggregate ever return tuple? Does this need to be made valid?
    # def test_invalid_left_term_error_tuple(self):
    #     program = """\
    #         (1,a) = #sum{ Y : p(Y,Z) } :- b(X,Z).
    #     """
    #     _, errors = self.rewrite(program)

    #     self.assertEqual(len(errors), 1)
    #     self.assertIn(
    #         "The left-hand side of an assignment must be a function term",
    #         errors[0].message,
    #     )
    #     self.assertIn("found (1,a)", errors[0].message)

    # #  Note: Is TermVariable valid on the left side of an assignment?
    # def test_invalid_left_term_error_variable(self):
    #     program = textwrap.dedent(
    #         """\
    #         X = #sum{ Y : p(Y,Z) } :- b(X,Z).
    #     """
    #     )
    #     _, errors = self.rewrite(program)
    #     self.assertEqual(len(errors), 1)
    #     self.assertIn(
    #         "The left-hand side of an assignment must be a function term",
    #         errors[0].message,
    #     )
    #     self.assertIn("found X", errors[0].message)

    def test_non_rule_statement_passthrough(self):
        program = "p(a)."

        stmts = self.parse_program(program)
        out = normalize_assignment_aggregates(self.lib, stmts)

        self.assertEqual(
            "\n".join(str(s) for s in out).strip(), "#program base.\np(a)."
        )

    # def test_error_message_and_information(self):

    #     program = "f(X) < #sum{ Y : p(Y,Z) : q(Y,Z) } :- b(X,Z)."

    #     stmts = self.parse_program(program)
    #     rewriter = HeadAggregateToBodyRewriteTransformer(self.lib)
    #     out = rewriter.rewrite_statements(stmts)

    #     self.assertEqual(len(rewriter.errors), 1)
    #     self.assertEqual(
    #         rewriter.errors[0].message,
    #         'aggregates with comparisons cannot not be used in the head, found "f(X) < #sum { Y: p(Y,Z): q(Y,Z) }", assignments are of the form "f(X) = #sum { Y: p(Y,Z): q(Y,Z) }"',
    #     )
    #     self.assertIs(rewriter.errors[0].information, ast.HeadAggregate)

    # def test_malformed_head_aggregate(self):
    #     program = textwrap.dedent(
    #         """
    #             #program base.
    #             #sum { Y: p(Y,Z): q(Y,Z) } :- b(X,Z)."""
    #     )

    #     stmts = self.parse_program(program)
    #     rewriter = HeadAggregateToBodyRewriteTransformer(self.lib)
    #     out = rewriter.rewrite_statements(stmts)

    #     # The original statement is returned unchanged
    #     self.assertEqual("\n".join(str(s) for s in out).strip(), program.strip())
    #     # An error should be recorded
    #     self.assertEqual(len(rewriter.errors), 1)
    #     self.assertEqual(
    #         "Missing the left-hand side of the assignment: #sum { Y: p(Y,Z): q(Y,Z) }",
    #         rewriter.errors[0].message,
    #     )

    # def test_other_functions(self):
    #     program = """\
    #     f(X) = #max{ Y : p(Y) } :- b(X).
    #     f(X) = #min{ Y : q(Y) } :- b(X).
    #     f(X) = #sum+{ Y : q(Y) } :- b(X).
    #     """
    #     expected = textwrap.dedent(
    #         """\
    #         #program base.
    #         f(X)=W :- b(X); W = #max { Y: p(Y) }.
    #         f(X)=W :- b(X); W = #min { Y: q(Y) }.
    #         f(X)=W :- b(X); W = #sum+ { Y: q(Y) }.
    #     """
    #     ).strip()
    #     self.assertRewriteEqual(program, expected)

    # def test_used_variables(self):
    #     program = """\
    #     f(W) = #max{ W2 : p(W2) } :- b(W).
    #     """
    #     expected = textwrap.dedent(
    #         """\
    #         #program base.
    #         f(W)=W3 :- b(W); W3 = #max { W2: p(W2) }.
    #     """
    #     ).strip()
    #     self.assertRewriteEqual(program, expected)

    # def test_general_program_rewrite(self):
    #     program = textwrap.dedent(
    #         """
    #         #program base.
    #         p(a).
    #         q(X) :- r(X).
    #         f(X) = #sum{ Y : p(Y,Z) : q(X), r(X) } :- b(X,Z).
    #         g(X) = a.
    #     """
    #     ).strip()

    #     expected = textwrap.dedent(
    #         """
    #         #program base.
    #         p(a).
    #         q(X) :- r(X).
    #         f(X)=W :- b(X,Z); W = #sum { Y: p(Y,Z), q(X), r(X) }.
    #         g(X)=a.
    #     """
    #     ).strip()

    #     result, errors = self.rewrite(program)

    #     self.assertEqual(errors, [])
    #     self.assertCountEqual(result, expected.splitlines())
