import unittest

from clingo import ast as clingo_ast

from fasp.syntax_tree._nodes import FASP_AST
from fasp.syntax_tree.parsing.parser import parse_string
from fasp.syntax_tree.rewritings.unnesting.literals import (
    UnnestFunctionsInLiteralsTransformer,
)
from fasp.util.ast import ELibrary, FreshVariableGenerator
from fasp.syntax_tree.collectors import SymbolSignature, collect_variables
from fasp.syntax_tree.rewritings.unnesting.rules import unnest_functions


class TestUnnestFunctionsTransformerLowLevel(unittest.TestCase):

    def setUp(self):
        self.elib = ELibrary()
        self.lib = self.elib.library

    def assertEqualUnnesting(
        self,
        node: FASP_AST,
        evaluable_functions: list[str],
        expected_node: FASP_AST | None,
        outer: bool = False,
        unnest_left_guard_equality: bool = False,
    ):
        eval_sigs = {
            SymbolSignature(name, int(arity))
            for name, arity in (s.split("/") for s in evaluable_functions)
        }

        variableGenerator = FreshVariableGenerator(collect_variables(node))
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            eval_sigs,
            variableGenerator,
            unnest_left_guard_equality=unnest_left_guard_equality,
        )
        new_node = transformer.unnest(
            node,
            outer,
        )
        if expected_node is not None:
            self.assertIsNotNone(new_node)
            self.assertEqual(new_node, expected_node, f"{new_node} != {expected_node}")
        else:
            self.assertIsNone(new_node)

    def assertEqualUnnestingTerm(
        self,
        term_str: str,
        evaluable_functions: list[str],
        expected_term_str: str | None,
        outer: bool = False,
    ):
        term = clingo_ast.parse_term(self.lib, term_str)
        expected_term = (
            clingo_ast.parse_term(self.lib, expected_term_str)
            if expected_term_str is not None
            else None
        )
        self.assertEqualUnnesting(term, evaluable_functions, expected_term, outer)

    def assertEqualUnnestingLiteral(
        self,
        literal_str: str,
        evaluable_functions: list[str],
        expected_literal_str: str | None,
        unnest_left_guard_equality: bool = False,
    ):
        literal = clingo_ast.parse_literal(self.lib, literal_str)
        expected_literal = (
            clingo_ast.parse_literal(self.lib, expected_literal_str)
            if expected_literal_str is not None
            else None
        )
        self.assertEqualUnnesting(
            literal,
            evaluable_functions,
            expected_literal,
            outer=False,
            unnest_left_guard_equality=unnest_left_guard_equality,
        )

    def test_function(self):
        self.assertEqualUnnestingTerm("a", ["a/0"], "FUN")
        self.assertEqualUnnestingTerm("a", ["a/0"], None, outer=True)
        self.assertEqualUnnestingTerm("a", [], None)
        self.assertEqualUnnestingTerm("f(a)", ["f/1"], "FUN")
        self.assertEqualUnnestingTerm("f(a)", ["f/1"], None, outer=True)
        self.assertEqualUnnestingTerm("f(a)", [], None)

    def test_symbolic_literal(self):
        self.assertEqualUnnestingLiteral("a(a)", ["a/0"], "a(FUN)")
        self.assertEqualUnnestingLiteral("a", [], None)
        self.assertEqualUnnestingLiteral("p(f(a))", ["f/1"], "p(FUN)")
        self.assertEqualUnnestingLiteral("p(f(a))", ["p/1"], None)

    def test_symbolic_equality(self):
        self.assertEqualUnnestingLiteral("a=a", ["a/0"], "a=FUN")
        self.assertEqualUnnestingLiteral("a=b", ["a/0"], None)
        self.assertEqualUnnestingLiteral("a=b", [], None)
        self.assertEqualUnnestingLiteral("a=b", ["a/0", "b/0"], "a=FUN")
        self.assertEqualUnnestingLiteral("b=a", ["a/0"], "a=b")

        self.assertEqualUnnestingLiteral("a+1=2", ["a/0"], "FUN+1=2")
        self.assertEqualUnnestingLiteral("a*1=2", ["a/0"], "FUN*1=2")
        self.assertEqualUnnestingLiteral("a/1=2", ["a/0"], "FUN/1=2")
        self.assertEqualUnnestingLiteral("a-1=2", ["a/0"], "FUN-1=2")
        self.assertEqualUnnestingLiteral("a**1=2", ["a/0"], "FUN**1=2")
        self.assertEqualUnnestingLiteral("|a|=2", ["a/0"], "|FUN|=2")
        self.assertEqualUnnestingLiteral("-a=2", ["a/0"], "-FUN=2")

        self.assertEqualUnnestingLiteral(
            "a=a", ["a/0"], "FUN=FUN2", unnest_left_guard_equality=True
        )
        self.assertEqualUnnestingLiteral(
            "a=b", ["a/0"], "FUN=b", unnest_left_guard_equality=True
        )
        self.assertEqualUnnestingLiteral(
            "a=b", ["a/0", "b/0"], "FUN=FUN2", unnest_left_guard_equality=True
        )
        self.assertEqualUnnestingLiteral(
            "b=a", ["a/0"], "FUN=b", unnest_left_guard_equality=True
        )

    def test_symbolic_inequality(self):
        self.assertEqualUnnestingLiteral("a<a", ["a/0"], "FUN<FUN2")
        self.assertEqualUnnestingLiteral("a<b", ["a/0"], "FUN<b")
        self.assertEqualUnnestingLiteral("a<b", [], None)
        self.assertEqualUnnestingLiteral("a<b", ["a/0", "b/0"], "FUN<FUN2")
        self.assertEqualUnnestingLiteral("b<a", ["a/0"], "b<FUN")


class TestUnnestFunctionsTransformer(unittest.TestCase):
    """
    Unit tests for the UnnestFunctionsTransformer.
    """

    def setUp(self):
        self.lib = ELibrary()

    def apply_unnesting_node(
        self,
        program: str,
        evaluable_functions: list[str],
        unnest_left_guard_equality: bool = False,
        allowed_in_negated_literals: bool = True,
    ):
        """
        Apply unnesting node-by-node and return (new_program_str, list[set[str]]).
        """
        eval_sigs = {
            SymbolSignature(name, int(arity))
            for name, arity in (s.split("/") for s in evaluable_functions)
        }

        stmts = parse_string(self.lib, program)
        stmts = stmts[1:]  # Skip #program base

        new_stmts = []
        per_rule_sets = []
        used_vars: set[str] = set()

        for stmt in stmts:
            # NOTE: Collect used variables for each rule
            used_vars = collect_variables(stmt)
            variableGenerator = FreshVariableGenerator(used_vars)
            new_stmt, comps = unnest_functions(
                self.lib.library,
                stmt,
                eval_sigs,
                variableGenerator,
                unnest_left_guard_equality=unnest_left_guard_equality,
                allowed_in_negated_literals=allowed_in_negated_literals,
            )

            new_stmts.append(str(new_stmt).strip() if new_stmt else str(stmt).strip())
            per_rule_sets.append({str(c) for c in comps})

        program_str = (
            "\n".join(new_stmts)
            .replace("#program base.\n", "")
            .replace("#program base.", "")
            .strip()
        )
        return program_str, per_rule_sets

    def assertEqualUnnesting(
        self,
        program: str,
        evaluable_functions: list[str],
        expected_program: str,
        expected_sets: list[set[str]],
        unnest_left_guard_equality: bool = False,
        allowed_in_negated_literals: bool = True,
    ):
        new_program, unnested_sets = self.apply_unnesting_node(
            program,
            evaluable_functions,
            unnest_left_guard_equality,
            allowed_in_negated_literals,
        )

        if expected_program is not None:
            expected_lines = [line.strip() for line in expected_program.splitlines()]
            actual_lines = [line.strip() for line in new_program.splitlines()]
            self.assertEqual(expected_lines, actual_lines)

        if expected_sets is not None:
            self.assertEqual(expected_sets, unnested_sets)

    def test_non_evaluable_symbolic_and_function(self):
        self.assertEqualUnnesting(
            "q(a,b).",
            [],
            "q(a,b).",
            [set()],
        )

    def test_non_evaluable_symbolic_and_function(self):
        self.assertEqualUnnesting(
            ":- q(a,b).",
            [],
            ":- q(a,b).",
            [set()],
        )

    def test_non_evaluable_symbolic_and_function_tuple(self):
        self.assertEqualUnnesting(
            "r((a,b)).",
            [],
            "r((a,b)).",
            [set()],
        )

    def test_head(self):
        self.assertEqualUnnesting(
            """p(g(h(a,b),c),d(X)) :- q(X).""",
            ["g/2", "h/2", "a/0"],
            """p(FUN3,d(X)) :- q(X).""",
            [
                {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"},
            ],
        )

    def test_body(self):
        self.assertEqualUnnesting(
            """:- p(g(h(a,b),c),d(X)).""",
            ["g/2", "h/2", "a/0"],
            """:- p(FUN3,d(X)).""",
            [
                {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"},
            ],
        )

    def test_absolute_unary_binary_operations(self):
        self.assertEqualUnnesting(
            "s(abs(-1), -(1), 1+2).",
            [],
            "s(abs(-1),-1,1+2).",
            [set()],
        )

    def test_absolute(self):
        self.assertEqualUnnesting(
            "p(|f(a)|).",
            ["f/1"],
            "p(|FUN|).",
            [{"f(a)=FUN"}],
        )

    def test_absolute_with_evaluable_function(self):
        self.assertEqualUnnesting(
            "s(abs(f(a))).",
            ["f/1"],
            "s(abs(FUN)).",
            [{"f(a)=FUN"}],
        )

    # def test_nested_functions_chain(self):
    #     self.assertEqualUnnesting(
    #         "r(g(h(f(a)))).",
    #         ["f/1", "h/1", "g/1"],
    #         "r(FUN3).",
    #         [{"f(a)=FUN", "h(FUN)=FUN2", "g(FUN2)=FUN3"}],
    #     )

    def test_predicates_are_not_functions(self):
        self.assertEqualUnnesting(
            "f(a) :- q.",
            ["f/1"],
            "f(a) :- q.",
            [set()],
        )
        self.assertEqualUnnesting(
            ":- f(a).",
            ["f/1"],
            ":- f(a).",
            [set()],
        )

    def test_mixed_head_and_body(self):
        self.assertEqualUnnesting(
            "p(f(a)) :- q(f(b)).",
            ["f/1"],
            "p(FUN) :- q(FUN2).",
            [{"f(a)=FUN", "f(b)=FUN2"}],
        )

    def test_no_evaluable_functions(self):
        self.assertEqualUnnesting(
            "r(f(a)).",
            [],
            "r(f(a)).",
            [set()],
        )

    def test_double_occurrence_same_function(self):
        self.assertEqualUnnesting(
            "s(f(a), f(a)).",
            ["f/1"],
            "s(FUN,FUN2).",
            [{"f(a)=FUN", "f(a)=FUN2"}],
        )

    def test_double_occurrence_same_function_and_symbolic_function(self):
        self.assertEqualUnnesting(
            "s(f(a), f(a)).",
            ["f/1", "a/0"],
            "s(FUN2,FUN4).",
            [{"a=FUN", "a=FUN3", "f(FUN3)=FUN4", "f(FUN)=FUN2"}],
        )

    def test_comparison(self):
        self.assertEqualUnnesting(
            ":- f(a)<f(b).",
            ["f/1", "a/0"],
            ":- FUN2<FUN3.",
            [{"a=FUN", "f(FUN)=FUN2", "f(b)=FUN3"}],
        )

    def test_comparison_rules_split(self):
        self.assertEqualUnnesting(
            ":- f(a) = f(b).",
            ["f/1", "a/0", "h/1"],
            ":- f(FUN)=FUN2.",
            [{"a=FUN", "f(b)=FUN2"}],
        )
        self.assertEqualUnnesting(
            ":- f(a) = f(b).",
            ["f/1", "a/0", "h/1"],
            ":- FUN2=FUN3.",
            [{"f(FUN)=FUN2", "a=FUN", "f(b)=FUN3"}],
            unnest_left_guard_equality=True,
        )

        self.assertEqualUnnesting(
            ":- f(b)<g(x).",
            ["f/1", "a/0", "h/1"],
            ":- FUN<g(x).",
            [{"f(b)=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- f(c) = a.",
            ["f/1", "a/0", "h/1"],
            ":- f(c)=FUN.",
            [{"a=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- f(c) != d(c).",
            ["f/1", "a/0", "h/1"],
            ":- FUN!=d(c).",
            [{"f(c)=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- f(c)<a.",
            ["f/1", "a/0", "h/1"],
            ":- FUN<FUN2.",
            [{"f(c)=FUN", "a=FUN2"}],
        )

    def test_non_binary_comparisons(self):
        self.assertEqualUnnesting(
            ":- f(a)<f(b)<f(c).",
            ["f/1"],
            ":- FUN<FUN2<FUN3.",
            [{"f(a)=FUN", "f(b)=FUN2", "f(c)=FUN3"}],
        )

    def test_not_allowed_in_negated_literals(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualUnnesting(
                ":- not p(f(a)).",
                ["f/1"],
                None,
                None,
                allowed_in_negated_literals=False,
            )
        self.assertEqual(
            str(cm.exception),
            "Evaluable functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found 'f(a)' at <string>:1:10-14.",
        )

        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualUnnesting(
                ":- not p(f(a)).",
                ["a/0"],
                None,
                None,
                allowed_in_negated_literals=False,
            )
        self.assertEqual(
            str(cm.exception),
            "Evaluable functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found 'a' at <string>:1:12-14.",
        )
        self.assertEqualUnnesting(
            ":- not p(f(a)).",
            [],
            ":- not p(f(a)).",
            [set()],
            allowed_in_negated_literals=False,
        )

        self.assertEqualUnnesting(
            ":- not not p(f(a)).",
            ["a/0"],
            ":- not not p(f(FUN)).",
            [{"not not a=FUN"}],
            allowed_in_negated_literals=False,
        )

        self.assertEqualUnnesting(
            ":- not not p(f(a)).",
            ["f/1"],
            ":- not not p(FUN).",
            [{"not not f(a)=FUN"}],
            allowed_in_negated_literals=False,
        )

        self.assertEqualUnnesting(
            ":- g = a.",
            ["f/1", "a/0", "h/1"],
            ":- a=g.",
            [set()],
        )

        self.assertEqualUnnesting(
            ":- g = f(1).",
            ["f/1", "a/0", "h/1"],
            ":- f(1)=g.",
            [set()],
        )

    def test_head_simple_assignment_unnesting(self):
        self.assertEqualUnnesting(
            "x(X) := h(f(X))+g(Y).",
            ["f/1", "g/1"],
            "x(X) := h(FUN)+FUN2.",
            [{"f(X)=FUN", "g(Y)=FUN2"}],
        )

    def test_double_negation(self):
        self.assertEqualUnnesting(
            "p(f(1),b) :- q(a,b), not not r(h(1)).",
            ["f/1", "a/0", "h/1"],
            "p(FUN,b) :- q(FUN2,b); not not r(FUN3).",
            [{"f(1)=FUN", "a=FUN2", "not not h(1)=FUN3"}],
        )

    def test_double_negation_in_multiple_level_unnesting(self):
        self.assertEqualUnnesting(
            "p :- not not q(f(g(a))).",
            ["f/1", "g/1", "a/0"],
            "p :- not not q(FUN3).",
            [{"not not a=FUN", "not not g(FUN)=FUN2", "not not f(FUN2)=FUN3"}],
        )

    def test_equality_comparison_in_head(self):
        self.assertEqualUnnesting(
            "a=b :- p.",
            ["a/0", "b/0"],
            "FUN=FUN2 :- p.",
            [{"a=FUN", "b=FUN2"}],
            unnest_left_guard_equality=True,
        )

    def test_negative_predicate(self):
        self.assertEqualUnnesting(
            "p :- q(a); not r(q(X)).",
            ["q/1", "a/0"],
            "p :- q(FUN); not r(FUN2).",
            [{"a=FUN", "q(X)=FUN2"}],
        )
