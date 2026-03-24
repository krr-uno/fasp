import textwrap
import unittest

from typing import List

from clingo import ast
from clingo.core import Library
from clingo.symbol import Number
from clingo.symbol import Symbol, Function as SymbolFunction

from asp2funasp.util import util
from asp2funasp.util.ast import AST, StatementAST

from tests.util import find_in_ast, parse_and_find, collect_statements


class UtilTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()


    def test_negate_operator_mapping(self) -> None:
        rel = ast.Relation
        self.assertEqual(util.negate_operator(rel.Equal), rel.NotEqual)
        self.assertEqual(util.negate_operator(rel.NotEqual), rel.Equal)
        self.assertEqual(util.negate_operator(rel.Less), rel.GreaterEqual)
        self.assertEqual(util.negate_operator(rel.LessEqual), rel.Greater)
        self.assertEqual(util.negate_operator(rel.Greater), rel.LessEqual)
        self.assertEqual(util.negate_operator(rel.GreaterEqual), rel.Less)

    def test_is_constraint_true_and_false(self) -> None:
        head = parse_and_find(self.lib, ":- 1 = X.", ast.HeadSimpleLiteral)
        assert isinstance(head, ast.HeadSimpleLiteral)
        self.assertTrue(util.is_constraint(head))

        # Parse a normal rule and ensure is_constraint returns False
        rules = collect_statements(self.lib, "a(X) :- base(X).")
        self.assertTrue(rules)
        rule = rules[0]
        self.assertFalse(util.is_constraint(rule.head))

    def test_extract_comparison_terms(self) -> None:
        comp = parse_and_find(self.lib, "p :- 1 = X.", ast.LiteralComparison)
        assert isinstance(comp, ast.LiteralComparison)
        self.assertIsNotNone(comp)
        lhs_terms, op, rhs_terms = util.extract_comparison_terms(comp)

        self.assertEqual(len(list(lhs_terms)), 1)
        self.assertEqual(len(list(rhs_terms)), 1)
        self.assertEqual(op, ast.Relation.Equal)

    def test_function_arguments_on_termfunction(self) -> None:
        termf = parse_and_find(self.lib, "q(f(1,2)).", ast.TermFunction)
        self.assertIsNotNone(termf)
        assert isinstance(termf, ast.TermFunction)  # for mypy, typecheck

        name, args = util.function_arguments(termf)
        self.assertEqual(name, termf.name)

        assert isinstance(termf.pool[0], ast.ArgumentTuple), f"got {type(termf.pool[0])}" # for mypy, typecheck
        # arguments should match pool[0].arguments
        self.assertEqual(list(args), list(termf.pool[0].arguments))

    def test_function_arguments_on_termtuple(self) -> None:
        termt = parse_and_find(self.lib, "q((1,2)).", ast.TermTuple)
        self.assertIsNotNone(termt)
        assert isinstance(termt, ast.TermTuple) # for mypy, typecheck
        name, args = util.function_arguments(termt)
        # term tuple should yield empty name and pool[0].arguments
        self.assertEqual(name, "")

        assert isinstance(termt.pool[0], ast.ArgumentTuple), f"got {type(termt.pool[0])}" # for mypy, typecheck
        self.assertEqual(list(args), list(termt.pool[0].arguments))

    def test_function_arguments_on_termsymbolic_function_symbol(self) -> None:
        # Build a symbolic Function and wrap in TermSymbolic
        ts = parse_and_find(self.lib, "q(g(a)).", ast.TermSymbolic)
        assert isinstance(ts, ast.TermSymbolic) # for mypy, typecheck
        name, args = util.function_arguments(ts)
        self.assertEqual(name, "a")
        self.assertEqual(list(args), [])

    def test_split_multiple_aggregate_elements(self) -> None:
        prog = textwrap.dedent("""
            { a(X); b(X) } :- c(X).
        """)
        rules = collect_statements(self.lib, prog)
        self.assertTrue(rules)
        rule = rules[0]
        new_rules = list(util.split_multiple_aggregate_elements(self.lib, rule))
        # There were two elements, so two rules should be returned
        self.assertEqual(len(new_rules), 2)
        for nr in new_rules:
            assert isinstance(nr, ast.StatementRule)
            assert isinstance(nr.head, ast.HeadSetAggregate)
            self.assertEqual(len(list(nr.head.elements)), 1)

    def test_same_bound_symbol_direct(self) -> None:
        left = parse_and_find(self.lib, "1 { a(X): p(X) } 1 :- .", ast.LeftGuard)
        right = parse_and_find(self.lib, "1 { a(X): p(X) } 1 :- .", ast.RightGuard)
        assert isinstance(left, ast.LeftGuard)
        assert isinstance(right, ast.RightGuard)
        self.assertTrue(util.same_bound_symbol(left, right))

    def test_identify_invariant_positions_and_get_variant_subsets(self) -> None:
        occurrences = [(1, 2, 3), (1, 4, 3), (1, 2, 3)]
        inv = util.identify_invariant_positions(occurrences)
        # positions: 0 -> invariant (all 1), 1 -> variant, 2 -> invariant
        self.assertEqual(inv, [1, 0, 1])

        variant_positions = [1, 2]
        lists = [["a", "b", "c"], ["x", "y", "z"]]
        subsets = util.get_variant_subsets(variant_positions, lists)
        # Each list yields subsets of its selected elements (positions 1 and 2)
        self.assertEqual(len(subsets), 2)
        # For two variant elements, number of non-empty subsets = 3
        self.assertEqual(len(subsets[0]), 3)

    def test_is_function(self) -> None:
        term = parse_and_find(self.lib, "1 { a(X): p(X) } 1 :- a.", ast.TermFunction)
        assert isinstance(term, ast.TermFunction)
        assert util.is_function(term)

    def test_get_parameter_list(self) -> None:
        literalSymbolic = parse_and_find(self.lib, "p(X) :- a.", ast.LiteralSymbolic)
        if literalSymbolic:
            self.assertEqual(str(literalSymbolic), "p(X)")
            params = util.get_parameter_list(literalSymbolic)
            self.assertEqual(params, ['X'])

        symbolic = parse_and_find(self.lib, "p(1,2).", ast.TermSymbolic)
        if symbolic:
            self.assertEqual(str(symbolic), "1")
            params = util.get_parameter_list(symbolic)
            self.assertEqual(params, [])


        termFunction = parse_and_find(self.lib, "1 { a(b(X),c): p(X) } 1 :- a.", ast.TermFunction)
        if termFunction:
            self.assertEqual(str(termFunction), "a(b(X),c)")
            params = util.get_parameter_list(termFunction)
            self.assertEqual(params, ['b(X)', 'c'])

        # termSymbolic = parse_and_find(self.lib, "{ a(X): p}:- a.", ast.TermSymbolic)
        # self.assertEqual(str(termSymbolic), "p")
        # params = util.get_parameter_list(termSymbolic)
        # self.assertEqual(params, [])


if __name__ == "__main__":
    unittest.main()

