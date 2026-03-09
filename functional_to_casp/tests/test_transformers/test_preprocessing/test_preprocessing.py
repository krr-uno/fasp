import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from casp.transformers.preprocessing import processPipelinetransformers
from casp.util.ast import AST

class NotAggregateConstraintTransformerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.transformer = processPipelinetransformers
        self.maxDiff = None

    def _apply(self, program: str) -> str:
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)
        rewritten_nodes = self.transformer(self.lib, nodes)
        rewritten = [str(node) for node in rewritten_nodes]
        # Remove the program declaration if present
        rewritten = rewritten[1:]
        return "\n".join(rewritten)

    def assertRewriteEqual(self, program: str, expected: str) -> None:
        result = self._apply(program)
        expected = textwrap.dedent(expected).strip()
        self.assertEqual(result.strip(), expected)

    # -----------------------------------------------------------
    # TESTS
    # -----------------------------------------------------------

    def test_non_constraint_rule_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            p(X) :- not 1 <= #count { X: a(X) }.
            """,
            """
            p(X) :- not 1 <= #count { X: a(X) }.
            """
        )

    def test_constraint_without_negated_aggregate_unchanged(self) -> None:
        self.assertRewriteEqual(
            """
            :- 1 <= #count { X: a(X) }.
            """,
            """
            :- 1 <= #count { X: a(X) }.
            """
        )

    def test_split_and_rewrite_aggregates(self) -> None:
        self.assertRewriteEqual(
            """
            0 <  { assign(N,C) : color(C) } <  2 :- node(N).
            1 <  { assign(N,C) : color(C) } <= 2 :- node(N).
            3 <= { assign(N,C) : color(C) } <  4 :- node(N).
            4    { assign(N,C) : color(C) }    4 :- node(N).
                 { assign(N,C) : color(C) } =  5 :- node(N).
            a :- not #count{ C : assign(N,C), color(C) } = 1, node(N).
            { assign(N,C) : color(C),a(X) } :- node(N).
            """,
            """
            { assign(N,C): color(C) } = 1 :- node(N).
            { assign(N,C): color(C) } = 2 :- node(N).
            { assign(N,C): color(C) } = 3 :- node(N).
            { assign(N,C): color(C) } = 4 :- node(N).
            { assign(N,C): color(C) } = 5 :- node(N).
            a :- not #count { C: assign(N,C), color(C) } = 1; node(N).
            { assign(N,C) } :- node(N); color(C); a(X).
            """
        )

    # TODO: Need to fix this?
    # def test_negation(self) -> None:
    #     self.assertRewriteEqual(
    #         ":- not #count{ C : assign(N,C), color(C) } = 1, node(N).",
    #         "#false :- 1 != #count { C: assign(N,C), color(C) }; node(N)."
    #     )
    def test_negation(self) -> None:
        self.assertRewriteEqual(
            ":- not 1 = #count{ C : assign(N,C), color(C) }, node(N).",
            ":- 1 != #count { C: assign(N,C), color(C) }; node(N)."
        )

    def test_split_head_aggregates_semicolon(self) -> None:
        self.assertRewriteEqual(
            """
            { a(X,Y) : b(X); c(X,Y) : d(X) } :- e(Y).
            """,
            """
            { a(X,Y) } :- e(Y); b(X).
            { c(X,Y) } :- e(Y); d(X).
            """
        )

    def test_split_head_aggregates_semicolon_and_comma(self) -> None:
        self.assertRewriteEqual(
            """
            { a(X,Y) : b(X); c(X,Y,Z) : d(X),e(Z) } :- e(Y).
            """,
            """
            { a(X,Y) } :- e(Y); b(X).
            { c(X,Y,Z) } :- e(Y); d(X); e(Z).
            """
        )

    def test_split_head_aggregates_disjunction(self) -> None:
        self.assertRewriteEqual(
            """
            { a(X,Y); c(X,Y,Z) : d(X),e(Z) } :- e(Y).
            """,
            """
            { a(X,Y) } :- e(Y).
            { c(X,Y,Z) } :- e(Y); d(X); e(Z).
            """
        )

    