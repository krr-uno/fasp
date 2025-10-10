import textwrap
import unittest

from fasp.ast.parsing.parser import parse_string
from fasp.util.ast import ELibrary
from fasp.ast.collectors import SymbolSignature
from fasp.ast.rewritings.unnesting import UnnestFunctionsTransformer


class TestUnnestFunctionsTransformer(unittest.TestCase):
    """
    Unit tests for the UnnestFunctionsTransformer.
    """

    def setUp(self):
        self.lib = ELibrary()

    def apply_unnesting(self, program: str, evaluable_functions: list[str]):
        """
        Helper: apply unnesting and return (new_program_str, list[set[str]]).
        """
        eval_sigs = set()
        for s in evaluable_functions:
            name, arity = s.split("/")
            eval_sigs.add(SymbolSignature(name, int(arity)))

        stmts = parse_string(self.lib, program)
        # Skip #program base.
        stmts = stmts[1:]

        new_stmts = []
        per_rule_sets: list[set[str]] = []

        for stmt in stmts:
            transformer = UnnestFunctionsTransformer(
                self.lib.library, eval_sigs, used_variable_names=set()
            )
            new_stmt = transformer.transform_rule(stmt)
            new_stmts.append(str(new_stmt).strip())

            unnested = set()
            for comp in transformer.unnested_functions:
                unnested.add(str(comp))
            per_rule_sets.append(unnested)

        # remove "#program base." if present
        program_str = "\n".join(new_stmts).replace("#program base.\n", "").replace("#program base.", "").strip()

        return program_str, per_rule_sets

    def assertEqualUnnesting(
        self,
        program: str,
        evaluable_functions: list[str],
        expected_program: str,
        expected_sets: list[set[str]],
    ):
        new_program, unnested_sets = self.apply_unnesting(program, evaluable_functions)

        # Compare programs
        expected_lines = [line.strip() for line in expected_program.splitlines()]
        actual_lines = [line.strip() for line in new_program.splitlines()]
        self.assertEqual(expected_lines, actual_lines)

        # Compare unnested sets
        self.assertEqual(expected_sets, unnested_sets)


    def test_unnest_example(self):
        self.assertEqualUnnesting(
            """p(g(h(a,b),c),d(X)) :- q(X).
               q(X) :- g(1,a) = X.
               g(X,Y) = Z :- true.""",
            ["g/2", "h/2", "a/0"],
            """p(FUN3,d(X)) :- q(X).
               q(X) :- g(1,FUN)=X.
               g(X,Y)=Z :- true.""",
            [
                {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"},
                {"a=FUN"},
                set(),
            ],
        )

    def test_non_evaluable_symbolic_and_function(self):
        self.assertEqualUnnesting(
            "q(a,b).",
            [],
            "q(a,b).",
            [set()],
        )

    def test_tuple(self):
        self.assertEqualUnnesting(
            "r((a,b)).",
            [],
            "r((a,b)).",
            [set()],
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

    def test_multiple_rules_and_variable_reuse(self):
        self.assertEqualUnnesting(
            """p(f(a)).
               q(f(b)).""",
            ["f/1"],
            """p(FUN).
               q(FUN).""",
            [
                {"f(a)=FUN"},
                {"f(b)=FUN"},
            ],
        )

    def test_nested_functions_chain(self):
        self.assertEqualUnnesting(
            "r(g(h(f(a)))).",
            ["f/1", "h/1", "g/1"],
            "r(FUN3).",
            [{"f(a)=FUN","h(FUN)=FUN2","g(FUN2)=FUN3"}]
        )

    def test_function_in_head_not_unnested(self):
        self.assertEqualUnnesting(
            "f(a) :- q.",
            ["f/1"],
            "f(a) :- q.",
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
            "s(FUN,FUN).",
            [{"f(a)=FUN"}],
        )

    def test_double_occurrence_same_function_and_symbolic_function(self):
        self.assertEqualUnnesting(
            "s(f(a), f(a)).",
            ["f/1", "a/0"],
            "s(FUN2,FUN2).",
            [{"a=FUN", "f(FUN)=FUN2"}],
        )

    def test_comparison(self):
        self.assertEqualUnnesting(
            ":- f(a) < f(b).",
            ["f/1", "a/0"],
            ":- FUN2<FUN3.",
            [{"a=FUN", "f(FUN)=FUN2", "f(b)=FUN3"}],
        )
    def test_comparison_rules_split(self):
        evaluable_functions = ["f/1", "a/0", "h/1"]

        self.assertEqualUnnesting(
            ":- f(a) = f(b).",
            evaluable_functions,
            ":- f(FUN)=FUN2.",
            [{"a=FUN", "f(b)=FUN2"}],
        )

        self.assertEqualUnnesting(
            ":- f(b) < g(x).",
            evaluable_functions,
            ":- FUN<g(x).",
            [{"f(b)=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- f(c) = a.",
            evaluable_functions,
            ":- f(c)=FUN.",
            [{"a=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- f(c) != d(c).",
            evaluable_functions,
            ":- FUN!=d(c).",
            [{"f(c)=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- f(c) < a.",
            evaluable_functions,
            ":- FUN<FUN2.",
            [{"f(c)=FUN", "a=FUN2"}],
        )

        self.assertEqualUnnesting(
            ":- #sum{ p(x) : f(x)< y(x) } = 1.",
            evaluable_functions,
            ":- #sum { p(x): FUN<y(x) } = 1.",
            [{"f(x)=FUN"}],
        )

        self.assertEqualUnnesting(
            ":- #count{ p(x) : f(z) = y(x) } = 0.",
            evaluable_functions,
            ":- #count { p(x): FUN=y(x) } = 0.",
            [{"f(z)=FUN"}],
        )

        self.assertEqualUnnesting(
            "p(f(1),b) :- q(a,b), not not r(h(1)).",
            evaluable_functions,
            "p(FUN,b) :- q(FUN2,b); not not r(FUN3).",
            [{"f(1)=FUN", "a=FUN2", "h(1)=FUN3"}],
        )

        self.assertEqualUnnesting(
            ":- g = a.",
            evaluable_functions,
            ":- a=g.",
            [set()],
        )

        self.assertEqualUnnesting(
            ":- g = f(1).",
            evaluable_functions,
            ":- f(1)=g.",
            [set()],
        )

    def test_aggregate_split(self):
        evaluable_functions = ["f/1", "a/1"]

        self.assertEqualUnnesting(
            ":- #sum { a(X): p(f(X)) } = 0.",
            evaluable_functions,
            ":- #sum { FUN: p(FUN2) } = 0.",
            [{"a(X)=FUN", "f(X)=FUN2"}],
        )

        self.assertEqualUnnesting(
            "#sum { a(X): p(f(X)) } = 0 :- p.",
            evaluable_functions,
            "#sum { a(X): p(FUN) } = 0 :- p.",
            [{"f(X)=FUN"}],
        )

    def test_aggregate_nested_split(self):
        self.assertEqualUnnesting(
            "f(X) = 1 :- b(X,Z), h(1) = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
            ["f/1", "a/1", "g/1", "h/1"],
            "f(X)=1 :- b(X,Z); FUN = #sum { FUN2: p(FUN3,Z), q(X), r(X) }.",
            [{"h(1)=FUN", "f(Y)=FUN2", "g(Y)=FUN3"}],
        )

    def test_aggregate_nested_split_with_assignment(self):
        self.assertEqualUnnesting(
            "f(X) := 1 :- b(X,Z), h(1) = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
            ["f/1", "a/1", "g/1", "h/1"],
            "f(X) := 1 :- b(X,Z); FUN = #sum { FUN2: p(FUN3,Z), q(X), r(X) }.",
            [{"h(1)=FUN", "f(Y)=FUN2", "g(Y)=FUN3"}],
        )
    # f(X)=1 vs f(X) := 1

    def test_assignment_unnesting(self):
        self.assertEqualUnnesting(
            "x(X) := h(f(X))+g(Y).",
            ["f/1", "g/1"],
            "x(X) := h(FUN)+g(Y).",
            [{"f(X)=FUN"}],
        )
    
    # TODO: Need to add more tests with assignments
