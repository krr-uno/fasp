from typing import List
import textwrap
import unittest

import re

from clingo_funasp import ast
from clingo_funasp.core import Library
from funasp.util.ast import AST
from asp2funasp.util.types import FRelation
from asp2funasp.rewriting.rewrite_into_funasp import (
    FunctionalPredicateRewriteTransformer,
)

from tests.util import collect_statements


class FunctionalPredicateRewriteTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    def _normalize(self, s: str) -> str:
        return re.sub(r"\s+", "", s)

    # APPLY TRANSFORMER
    def _rewrite(
        self,
        program: str,
        frels: List[FRelation],
    ) -> List[AST]:
        program = textwrap.dedent(program).strip()

        nodes: List[AST] = collect_statements(self.lib, program)

        transformer = FunctionalPredicateRewriteTransformer(self.lib, frels)

        new_nodes: List[AST] = []

        for node in nodes:
            new_node = transformer.transform_rule(node)

            if new_node is None:
                new_nodes.append(node)
            else:
                new_nodes.append(new_node)

        return new_nodes

    # ASSERT HELPER
    def assertEqualRewrite(
        self,
        program: str,
        expected: str,
        frels: List[FRelation],
    ):
        actual_nodes = self._rewrite(program, frels)
        # expected_nodes = collect_statements(
        #     self.lib,
        #     textwrap.dedent(expected).strip(),
        # )

        # Compare via string form (standard in clingo AST tests)
        actual_str = self._normalize("\n".join(str(n) for n in actual_nodes))
        # expected_str = "\n".join(str(n) for n in expected_nodes)
        expected_str = self._normalize(textwrap.dedent(expected).strip())

        self.assertEqual(
            actual_str,
            expected_str,
            msg=f"\nEXPECTED:\n{expected_str}\n\nACTUAL:\n{actual_str}",
        )

    def _make_non_function_symbolic_literal(self) -> ast.LiteralSymbolic:
        nodes = collect_statements(self.lib, "a.")
        rule = nodes[0]
        assert isinstance(rule, ast.StatementRule)

        variable_atom = ast.TermVariable(
            self.lib,
            rule.location,
            "X",
        )

        return ast.LiteralSymbolic(
            self.lib,
            rule.location,
            ast.Sign.NoSign,
            variable_atom,
        )

    ## TESTS ##

    def test_no_functions(self):
        program = ":- a."
        expected = ":- a."
        frels = []

        self.assertEqualRewrite(program, expected, frels)

    def test_simple_body_rewrite(self):
        program = """
        :- assign(N,C); node(N).
        """

        expected = """
        :-Fassign(N,C); node(N).
        """

        # expected = """
        # :- assign(N) = C, node(N).
        # """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_aggregate_rewrite(self):
        program = """
        :- #count { C,N: assign(N,C) } != 1; node(N).
        """

        expected = """
        :- #count { C,N: Fassign(N,C) } != 1; node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_functional_predicate_in_simple_head(self):
        program = """
        p(X,Y) :- q(X,Y).
        """

        expected = """
        Fp(X, Y) :- q(X,Y).
        """

        # expected = """
        # p(X) := Y :- q(X,Y).
        # """

        frels = [
            FRelation(
                name="p",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    ## TODO: Check if this is correct
    def test_does_not_rewrite_disjunction_head(self):
        """
        When a predicate occurs in a disjunction head, the predicate is not rewritten into a functional predicate, even if it is functional, in any rule of the program.
        That is, we remove it from the list of functional predicates for the rewriting of that program.
        """
        program = """
        p(X,Y) | q(X,Y) :- r(X,Y).
        """

        expected = """
        p(X,Y); q(X,Y) :- r(X,Y).
        """

        frels = [
            FRelation(
                name="p",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_does_not_rewrite_head_literal_if_atom_is_not_function(self):
        literal = self._make_non_function_symbolic_literal()

        head = ast.HeadSimpleLiteral(
            self.lib,
            literal,
        )

        transformer = FunctionalPredicateRewriteTransformer(
            self.lib,
            [
                FRelation(
                    name="p",
                    arity=2,
                    arguments=(0,),
                    values=[(1,)],
                )
            ],
        )

        self.assertIsNone(transformer._rewrite_head(head))

    def test_rewrites_head_set_aggregate_element(self):
        program = """
        { assign(N,C) : color(C) } :- node(N).
        """

        expected = """
        { Fassign(N,C): color(C) } :- node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_head_set_aggregate_without_changes_remains_unchanged(self):
        program = """
        { color(C) } :- node(N).
        """

        expected = """
        { color(C) } :- node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_nonfunctional_head_set_aggregate_element_condition(self):
        program = """
        { color(C) : assign(N,C) } :- node(N).
        """

        expected = """
        { color(C): Fassign(N, C) } :- node(N).
        """

        # expected = """
        # { color(C): assign(N) = C } :- node(N).
        # """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_comparison_head_set_aggregate_element_condition(self):
        program = """
        { C = 1 : assign(N,C) } :- node(N).
        """

        expected = """
        { C = 1: Fassign(N, C) } :- node(N).
        """

        # expected = """
        # { C = 1: assign(N) = C } :- node(N).
        # """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_head_aggregate_element(self):

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
        ]

        program = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        """

        expected = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        """


        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_with_conflicts_1(self):

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        program = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        assign(N,C,V).
        """

        # How should this be transformed?
        expected = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        Fassign_1(N,C,V).
        """

        # assign_1(N,C) := V.

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_with_conflicts_2(self):

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        program = """
        color(assign(N,C)) :- node(N); c(C).
        assign(N,C,V).
        """

        # How should this be transformed?
        expected = """
        color(assign(N,C)) :- node(N); c(C).
        Fassign_1(N,C,V).
        """

        # assign(N,C) := V.



        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_with_conflicts_3(self):

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            ),
            FRelation(
                name="assign",
                arity=4,
                arguments=(0, 1, 2),
                values=[(3,)],
            ),
        ]

        program = """
        color(assign(N,C)) :- node(N); c(C).
        assign(N,C,V).
        assign(N,C,V,W).
        """

        expected = """
        color(assign(N,C)) :- node(N); c(C).
        Fassign_1(N,C,V).
        Fassign_2(N,C,V,W).
        """

        # no conflict for Fassign_2(N,C,V,W).

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_with_conflicts_4(self):
        program = """
        assign(N,C,V).
        assign(N,C,V,W).
        """

        expected = """
        Fassign_1(N,C,V).
        Fassign_2(N,C,V,W).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
            FRelation(
                name="assign",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            ),
            FRelation(
                name="assign",
                arity=4,
                arguments=(0, 1, 2),
                values=[(3,)],
            ),
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_head_aggregate_element_condition(self):
        program = """
        #count { C: q(C): assign(N,C) } = 1 :- node(N).
        """

        expected = """
        #count { C: q(C): Fassign(N, C) } = 1 :- node(N).
        """

        # expected = """
        # #count { C: q(C): assign(N) = C } = 1 :- node(N).
        # """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_head_conditional_literal_condition(self):
        program = """
        p(X): assign(N,C).
        """

        expected = """
        p(X): Fassign(N,C).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_head_conditional_literal_literal(self):
        program = """
        assign(N,C): p(X).
        """

        expected = """
        Fassign(N,C): p(X).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_does_not_rewrites_head_conditional_literal_condition_if_nothing_functional(self):
        program = """
        p(X): assign(N,C).
        """

        expected = """
        p(X): assign(N,C).
        """

        frels = []

        self.assertEqualRewrite(program, expected, frels)



    ## EXTRA TESTS ##
    def test_no_rewrite_if_not_functional_body(self):
        program = """
        :- assign(N,C); node(N); a.
        """

        expected = """
        :- assign(N,C); node(N); a.
        """

        frels: List[FRelation] = []  # nothing functional

        self.assertEqualRewrite(program, expected, frels)

    def test_multiple_args_function(self):
        program = """
        :- f(X,Y,Z).
        """

        expected = """
        :- Ff(X,Y,Z).
        """

        # expected = """
        # :- f(X,Y) = Z.
        # """

        frels = [
            FRelation(
                name="f",
                arity=3,
                arguments=(0, 1),
                values=[(2,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_negated_literal(self):
        program = """
        :- not assign(N,C).
        """

        expected = """
        :- not Fassign(N,C).
        """

        # expected = """
        # :- not assign(N) = C.
        # """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_no_rewrite_if_not_functional_head(self):
        program = """
        p(X,Y) :- q(X,Y).
        """

        expected = """
        p(X,Y) :- q(X,Y).
        """

        frels: List[FRelation] = []  # nothing functional

        self.assertEqualRewrite(program, expected, frels)

    def test_does_not_rewrite_body_literal_if_atom_is_not_function(self):
        literal = self._make_non_function_symbolic_literal()

        transformer = FunctionalPredicateRewriteTransformer(
            self.lib,
            [
                FRelation(
                    name="p",
                    arity=2,
                    arguments=(0,),
                    values=[(1,)],
                )
            ],
        )

        self.assertIsNone(transformer._rewrite(literal))


    def test_comparison_head_set_aggregate_without_changes_remains_unchanged(self):
        program = """
        { C = 1 } :- node(N).
        """

        expected = """
        { C = 1 } :- node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            )
        ]

        self.assertEqualRewrite(program, expected, frels)
