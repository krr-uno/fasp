from typing import List
import textwrap
import unittest

from clingo import ast

from funasp.util.ast import AST, ELibrary
from asp2funasp.util.types import FRelation
from asp2funasp.rewriting.rewrite_into_funasp import (
    FunctionalPredicateRewriteTransformer,
)

from tests.util import collect_statements_funasp


class FunctionalPredicateRewriteTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = ELibrary()

    # APPLY TRANSFORMER
    def _rewrite(
        self,
        program: str,
        frels: List[FRelation],
    ) -> List[AST]:
        program = textwrap.dedent(program).strip()

        nodes: List[AST] = collect_statements_funasp(self.lib, program)

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
        expected_nodes = collect_statements_funasp(
            self.lib,
            textwrap.dedent(expected).strip(),
        )

        # Compare via string form (standard in clingo AST tests)
        actual_str = "\n".join(str(n) for n in actual_nodes)
        expected_str = "\n".join(str(n) for n in expected_nodes)

        self.assertEqual(
            actual_str,
            expected_str,
            msg=f"\nEXPECTED:\n{expected_str}\n\nACTUAL:\n{actual_str}",
        )

    def _make_non_function_symbolic_literal(self) -> ast.LiteralSymbolic:
        nodes = collect_statements_funasp(self.lib, "a.")
        rule = nodes[0]
        assert isinstance(rule, ast.StatementRule)

        variable_atom = ast.TermVariable(
            self.lib.library,
            rule.location,
            "X",
        )

        return ast.LiteralSymbolic(
            self.lib.library,
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
        :- assign(N,C), node(N).
        """

        expected = """
        :- assign(N) = C, node(N).
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

    def test_aggregate_rewrite(self):
        program = """
        :- #count{ C,N : assign(N,C) } != 1, node(N).
        """

        expected = """
        :- #count{ C,N : assign(N) = C } != 1, node(N).
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

    def test_no_rewrite_if_not_functional_body(self):
        program = """
        :- assign(N,C), node(N), a.
        """

        expected = """
        :- assign(N,C), node(N), a.
        """

        frels: List[FRelation] = []  # nothing functional

        self.assertEqualRewrite(program, expected, frels)

    def test_multiple_args_function(self):
        program = """
        :- f(X,Y,Z).
        """

        expected = """
        :- f(X,Y) = Z.
        """

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
        :- not assign(N) = C.
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
        p(X) := Y :- q(X,Y).
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

    def test_no_rewrite_if_not_functional_head(self):
        program = """
        p(X,Y) :- q(X,Y).
        """

        expected = """
        p(X,Y) :- q(X,Y).
        """

        frels: List[FRelation] = []  # nothing functional

        self.assertEqualRewrite(program, expected, frels)

    def test_does_not_rewrite_disjunction_head(self):
        program = """
        p(X,Y) | q(X,Y) :- r(X,Y).
        """

        expected = """
        p(X,Y) | q(X,Y) :- r(X,Y).
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

    def test_does_not_rewrite_signed_head_literal(self):
        nodes = collect_statements_funasp(
            self.lib,
            textwrap.dedent(
                """
                :- not p(X,Y).
                """
            ).strip(),
        )

        rule = nodes[0]
        assert isinstance(rule, ast.StatementRule)

        signed_literal = rule.body[0].literal
        assert isinstance(signed_literal, ast.LiteralSymbolic)
        self.assertNotEqual(signed_literal.sign, ast.Sign.NoSign)

        head = ast.HeadSimpleLiteral(
            self.lib.library,
            signed_literal,
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

    def test_does_not_rewrite_head_literal_if_atom_is_not_function(self):
        literal = self._make_non_function_symbolic_literal()

        head = ast.HeadSimpleLiteral(
            self.lib.library,
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

    def test_rewrites_head_set_aggregate_element(self):
        program = """
        { assign(N,C) : color(C) } :- node(N).
        """

        expected = """
        { assign(N) := C: color(C) } :- node(N).
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
        { color(C): assign(N) = C } :- node(N).
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

    def test_rewrites_comparison_head_set_aggregate_element_condition(self):
        program = """
        { C = 1 : assign(N,C) } :- node(N).
        """

        expected = """
        { C = 1: assign(N) = C } :- node(N).
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

    def test_rewrites_head_aggregate_element(self):
        program = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        """

        # How should this be transformed?
        expected = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        """

        frels = [
            FRelation(
                name="assign",
                arity=2,
                arguments=(0,),
                values=[(1,)],
            ),
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_with_conflicts_1(self):
        program = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        assign_1(N,C,V).
        """

        # How should this be transformed?
        expected = """
        #count { assign(N,C): color(C) } = 1 :- node(N).
        assign_1(N,C) := V.
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
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_with_conflicts_2(self):
        program = """
        color(assign(N,C)) :- node(N), c(C).
        assign_1(N,C,V).
        """

        # How should this be transformed?
        expected = """
        color(assign(N,C)) :- node(N), c(C).
        assign_1(N,C) := V.
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
            )
        ]

        self.assertEqualRewrite(program, expected, frels)

    def test_rewrites_head_aggregate_element_condition(self):
        program = """
        #count { C: q(C): assign(N,C) } = 1 :- node(N).
        """

        expected = """
        #count { C: q(C): assign(N) = C } = 1 :- node(N).
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

    def test_rewrites_head_conditional_literal_condition(self):
        nodes = collect_statements_funasp(self.lib, "p(X): assign(N,C).")
        expected_nodes = collect_statements_funasp(self.lib, "p(X): assign(N) = C.")

        rule = nodes[0]
        expected_rule = expected_nodes[0]
        assert isinstance(rule, ast.StatementRule)
        assert isinstance(expected_rule, ast.StatementRule)

        conditional_literal = rule.head.elements[0]
        expected_conditional_literal = expected_rule.head.elements[0]

        transformer = FunctionalPredicateRewriteTransformer(
            self.lib,
            [
                FRelation(
                    name="assign",
                    arity=2,
                    arguments=(0,),
                    values=[(1,)],
                )
            ],
        )

        new_conditional_literal, changed = (
            transformer._rewrite_head_conditional_literal(conditional_literal)
        )

        self.assertTrue(changed)
        self.assertEqual(str(new_conditional_literal), str(expected_conditional_literal))

    def test_head_conditional_literal_without_changes_remains_unchanged(self):
        nodes = collect_statements_funasp(self.lib, "p(X): q(X).")

        rule = nodes[0]
        assert isinstance(rule, ast.StatementRule)

        conditional_literal = rule.head.elements[0]

        transformer = FunctionalPredicateRewriteTransformer(
            self.lib,
            [
                FRelation(
                    name="assign",
                    arity=2,
                    arguments=(0,),
                    values=[(1,)],
                )
            ],
        )

        new_conditional_literal, changed = (
            transformer._rewrite_head_conditional_literal(conditional_literal)
        )

        self.assertFalse(changed)
        self.assertEqual(str(new_conditional_literal), str(conditional_literal))
