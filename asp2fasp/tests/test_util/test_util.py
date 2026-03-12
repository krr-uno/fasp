import textwrap
import unittest

from clingo import ast
from clingo.core import Library, Location
from clingo.symbol import Number
from clingo.symbol import Symbol, Function as SymbolFunction

from asp2fasp.util import util


def _collect_ast_nodes(nodes):
    """Helper to collect StatementRule nodes from parsed AST nodes."""
    return [n for n in nodes if isinstance(n, ast.StatementRule)]

def find_in_ast(node, typ):
    result = None

    def visitor(n):
        nonlocal result

        if result is None and isinstance(n, typ):
            result = n
            return  # stop early if desired

        # recurse into children
        n.visit(visitor)

    visitor(node)
    return result

def parse_and_find(lib: Library, program: str, typ: type):
    nodes = []
    ast.parse_string(lib, program, nodes.append)
    rules = _collect_ast_nodes(nodes)

    if not rules:
        return None

    return find_in_ast(rules[0], typ)


class UtilTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()


    def test_negate_operator_mapping(self):
        rel = ast.Relation
        self.assertEqual(util.negate_operator(rel.Equal), rel.NotEqual)
        self.assertEqual(util.negate_operator(rel.NotEqual), rel.Equal)
        self.assertEqual(util.negate_operator(rel.Less), rel.GreaterEqual)
        self.assertEqual(util.negate_operator(rel.LessEqual), rel.Greater)
        self.assertEqual(util.negate_operator(rel.Greater), rel.LessEqual)
        self.assertEqual(util.negate_operator(rel.GreaterEqual), rel.Less)

    def test_is_constraint_true_and_false(self):
        head = parse_and_find(self.lib, ":- 1 = X.", ast.HeadSimpleLiteral)
        self.assertTrue(util.is_constraint(head))

        # Parse a normal rule and ensure is_constraint returns False
        nodes = []
        ast.parse_string(self.lib, "a(X) :- base(X).", nodes.append)
        rules = _collect_ast_nodes(nodes)
        self.assertTrue(rules)
        rule = rules[0]
        self.assertFalse(util.is_constraint(rule.head))

    def test_extract_comparison_terms(self):
        comp = parse_and_find(self.lib, "p :- 1 = X.", ast.LiteralComparison)
        self.assertIsNotNone(comp)
        lhs_terms, op, rhs_terms = util.extract_comparison_terms(comp)

        self.assertEqual(len(list(lhs_terms)), 1)
        self.assertEqual(len(list(rhs_terms)), 1)
        self.assertEqual(op, ast.Relation.Equal)

    def test_function_arguments_on_termfunction(self):
        termf = parse_and_find(self.lib, "q(f(1,2)).", ast.TermFunction)
        self.assertIsNotNone(termf)
        name, args = util.function_arguments(termf)
        self.assertEqual(name, termf.name)
        # arguments should match pool[0].arguments
        self.assertEqual(list(args), list(termf.pool[0].arguments))

    def test_function_arguments_on_termtuple(self):
        termt = parse_and_find(self.lib, "q((1,2)).", ast.TermTuple)
        self.assertIsNotNone(termt)
        name, args = util.function_arguments(termt)
        # term tuple should yield empty name and pool[0].arguments
        self.assertEqual(name, "")
        self.assertEqual(list(args), list(termt.pool[0].arguments))

    def test_function_arguments_on_termsymbolic_function_symbol(self):
        # Build a symbolic Function and wrap in TermSymbolic
        ts = parse_and_find(self.lib, "q(g(a)).", ast.TermSymbolic)
        name, args = util.function_arguments(ts)
        self.assertEqual(name, "a")
        self.assertEqual(list(args), [])

    def test_split_multiple_aggregate_elements(self):
        prog = textwrap.dedent("""
            { a(X); b(X) } :- c(X).
        """)
        nodes = []
        ast.parse_string(self.lib, prog, nodes.append)
        rules = _collect_ast_nodes(nodes)
        self.assertTrue(rules)
        rule = rules[0]
        new_rules = list(util.split_multiple_aggregate_elements(self.lib, rule))
        # There were two elements, so two rules should be returned
        self.assertEqual(len(new_rules), 2)
        for nr in new_rules:
            self.assertIsInstance(nr.head, ast.HeadSetAggregate)
            self.assertEqual(len(list(nr.head.elements)), 1)

    def test_same_bound_symbol_direct(self):
        left = parse_and_find(self.lib, "1 { a(X): p(X) } 1 :- .", ast.LeftGuard)
        right = parse_and_find(self.lib, "1 { a(X): p(X) } 1 :- .", ast.RightGuard)
        self.assertTrue(util.same_bound_symbol(left, right))

    def test_identify_invariant_positions_and_get_variant_subsets(self):
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


if __name__ == "__main__":
    unittest.main()

