import unittest

from fasp.ast.parsing.parser import parse_string
from fasp.util.ast import ELibrary, FreshVariableGenerator
from fasp.ast.collectors import SymbolSignature, collect_variables
from fasp.ast.rewritings.unnesting import UnnestFunctionsTransformer, unnest_functions


class TestUnnestFunctionsTransformer(unittest.TestCase):
    """
    Unit tests for the UnnestFunctionsTransformer.
    """

    def setUp(self):
        self.lib = ELibrary()

    # def apply_unnesting(self, program: str, evaluable_functions: list[str]):
    #     """
    #     Helper: apply unnesting and return (new_program_str, list[set[str]]).
    #     """
    #     eval_sigs = set()
    #     for s in evaluable_functions:
    #         name, arity = s.split("/")
    #         eval_sigs.add(SymbolSignature(name, int(arity)))

    #     stmts = parse_string(self.lib, program)
    #     # Skip #program base.
    #     stmts = stmts[1:]

    #     new_stmts = []
    #     per_rule_sets: list[set[str]] = []

    #     for stmt in stmts:
    #         transformer = UnnestFunctionsTransformer(
    #             self.lib.library, eval_sigs, FreshVariableGenerator(set())
    #         )
    #         new_stmt = transformer.transform_rule(stmt)
    #         new_stmts.append(str(new_stmt).strip())

    #         unnested = set()
    #         for comp in transformer.unnested_functions:
    #             unnested.add(str(comp))
    #         per_rule_sets.append(unnested)

    #     # remove "#program base." if present
    #     program_str = "\n".join(new_stmts).replace("#program base.\n", "").replace("#program base.", "").strip()

    #     return program_str, per_rule_sets

    def apply_unnesting_node(self, program: str, evaluable_functions: list[str]):
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
                self.lib.library, stmt, eval_sigs, variableGenerator
            )

            new_stmts.append(str(new_stmt).strip())
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
    ):
        new_program, unnested_sets = self.apply_unnesting_node(
            program, evaluable_functions
        )

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
               p(X,Y) = Z :- true.""",
            ["g/2", "h/2", "a/0"],
            """p(FUN3,d(X)) :- q(X).
               q(X) :- g(1,FUN)=X.
               p(X,Y)=Z :- true.""",
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
            [{"f(a)=FUN", "h(FUN)=FUN2", "g(FUN2)=FUN3"}],
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
            ":- f(a) < f(b).",
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
            ":- f(b) < g(x).",
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
            ":- f(c) < a.",
            ["f/1", "a/0", "h/1"],
            ":- FUN<FUN2.",
            [{"f(c)=FUN", "a=FUN2"}],
        )

        self.assertEqualUnnesting(
            "#sum{ f(x) : h(x)< y(x) } = 1.",
            ["f/1", "a/0", "h/1"],
            "#sum { FUN: FUN2<y(x) } = 1.",
            [{"h(x)=FUN2", "f(x)=FUN"}],  # f(x) should be unnested
        )

        self.assertEqualUnnesting(
            ":- #sum{ f(x) : h(x)< y(x) } = 1.",
            ["f/1", "a/0", "h/1"],
            ":- #sum { FUN: FUN2<y(x) } = 1.",
            [{"f(x)=FUN", "h(x)=FUN2"}],
        )

        self.assertEqualUnnesting(
            ":- #count{ p(x) : h(f(z)) = y(x) } = 0.",
            ["f/1", "a/0", "h/1"],
            ":- #count { p(x): h(FUN)=y(x) } = 0.",
            [{"f(z)=FUN"}],
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

    def test_aggregate_split(self):
        self.assertEqualUnnesting(
            ":- #sum { a(X): p(f(X)) } = 0.",
            ["f/1", "a/1", "p/1"],
            ":- #sum { FUN: p(FUN2) } = 0.",
            [{"a(X)=FUN", "f(X)=FUN2"}],
        )

    def test_aggregate_split2(self):
        self.assertEqualUnnesting(
            "#sum { a(X): p(f(X)) } = 0 :- p.",
            ["f/1", "a/1", "p/1"],
            "#sum { FUN: p(FUN2) } = 0 :- p.",
            [{"a(X)=FUN", "f(X)=FUN2"}],
        )

    # f(X)=1 vs f(X) := 1 => SPACING
    def test_aggregate_nested_split_with_assignment(self):
        self.assertEqualUnnesting(
            "f(X) := 1 :- b(X,Z), h(1) = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
            ["f/1", "a/1", "g/1", "h/1"],
            "f(X) := 1 :- b(X,Z); FUN = #sum { FUN2: p(FUN3,Z), q(X), r(X) }.",
            [{"h(1)=FUN", "f(Y)=FUN2", "g(Y)=FUN3"}],
        )

    def test_head_simple_assignment_unnesting(self):
        self.assertEqualUnnesting(
            "x(X) := h(f(X))+g(Y).",
            ["f/1", "g/1"],
            "x(X) := h(FUN)+FUN2.",
            [{"f(X)=FUN", "g(Y)=FUN2"}],
        )

    def test_choice_rule(self):
        self.assertEqualUnnesting(
            "1 <= { a(X): f(a(Y))}.", ["a/1"], "1 <= { a(X): f(FUN) }.", [{"a(Y)=FUN"}]
        )

    def test_choice_rule_with_assignment_aggregate(self):
        self.assertEqualUnnesting(
            "1 <= { f(X) := 1: p(a); f(X) := 2: q(X) } <= 3.",
            ["f/1", "a/0"],
            "1 <= { f(X) := 1: p(FUN); f(X) := 2: q(X) } <= 3.",
            [{"a=FUN"}],
        )

    def test_assignment_with_aggregate(self):
        self.assertEqualUnnesting(
            "score(X) := #sum{ f(Y) : f(p(Y)), q(X) } :- p.",
            ["f/1", "p/1"],
            "score(X) := #sum{f(Y): f(FUN), q(X)} :- p.",
            [{"p(Y)=FUN"}],  # f(Y) should not be unnested
        )

    # def test_negative_predicate_in_aggregate(self):
    #     self.assertEqualUnnesting(
    #         "p :- #sum { X: q(a), r(q(X)) } = 1.",
    #         ["q/1", "a/0"],
    #         "p :- #sum { X: q(FUN), r(FUN2) } = 1.",
    #         [{"a=FUN", "q(X)=FUN2"}],
    #     )

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
    
    # CHECK: Comparison with equality in head.
    def test_equality_comparison_in_head(self):
        self.assertEqualUnnesting(
            "a=b :- p.",
            ["a/0", "b/0"],
            "FUN=FUN2 :- p.",
            [{"a=FUN", "b=FUN2"}],
        )

    # CHECK: Comparison with equality in head.
    # def test_aggregate_nested_split(self):
    #     self.assertEqualUnnesting(
    #         "f(X)=W :- b(X,Z), W = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
    #         ["f/1", "g/1", "h/1"],
    #         "f(X)=W :- b(X,Z); W = #sum { FUN: p(FUN2,Z), q(X), r(X) }.",
    #         [{"f(Y)=FUN", "g(Y)=FUN2"}],
    #     )

    #     self.assertEqualUnnesting(
    #         "f(X)=1 :- b(X,Z), h(1) = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
    #         ["f/1", "g/1", "h/1"],
    #         "f(X)=1 :- b(X,Z); FUN = #sum { FUN2: p(FUN3,Z), q(X), r(X) }.",
    #         [{"h(1)=FUN", "f(Y)=FUN2", "g(Y)=FUN3"}],
    #     )

    def test_aggregate_nested_split(self):
        self.assertEqualUnnesting(
            "f(X)=W :- b(X,Z), W = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
            ["f/1", "g/1", "h/1"],
            "FUN=W :- b(X,Z); W = #sum { FUN2: p(FUN3,Z), q(X), r(X) }.",
            [{"f(X)=FUN", "f(Y)=FUN2", "g(Y)=FUN3"}],
        )

        self.assertEqualUnnesting(
            "f(X)=1 :- b(X,Z), h(1) = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.",
            ["f/1", "g/1", "h/1"],
            "FUN=1 :- b(X,Z); FUN2 = #sum { FUN3: p(FUN4,Z), q(X), r(X) }.",
            [{"f(X)=FUN","h(1)=FUN2", "f(Y)=FUN3", "g(Y)=FUN4"}],
        )


########################################
    # NEED TO WORK ON THESE:
    def test_negative_predicate_in_body_aggregate_with_evaluable(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualUnnesting(
                "p :- #sum { X: q(a), not r(q(X)) } = 1.",
                ["q/1", "a/0"],
                "p :- #sum { X: q(FUN), r(FUN2) } = 1.",
                [{"a=FUN", "q(X)=FUN2"}]
            )
        self.assertEqual(str(cm.exception), "Evaluable functions are not allowed in negated literals in aggregates and body condition. Found q(X) at <string>:1:28-32.")
    #     NOT ALLOWED because not r(q(X)) has an evaluable function inside a negation in a body condition. The same applies to conditional literals "p :- q(a): not r(q(X)).",

    def test_negative_predicate_in_body_aggregate_without_evaluable(self):
        self.assertEqualUnnesting(
            "p :- #sum { X: q(a), not r(q(X)) } = 1.",
            ["f/1", "a/0"],
            "p :- #sum { X: q(FUN), not r(q(X)) } = 1.",
            [{"a=FUN"}]
        )

    def test_negative_predicate(self):
        self.assertEqualUnnesting(
            "p :- q(a); not r(q(X)).",
            ["q/1", "a/0"],
            "p :- q(FUN); not r(FUN2).",
            [{"a=FUN", "q(X)=FUN2"}]
        )
    #     THIS IS ALLOWED because not r(q(X)) is not inside the aggregate or conditional literal

    def test_negative_predicate_in_aggregate_with_evaluable_head_aggregate(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualUnnesting(
                "#sum { a(X): not p(f(X)): p(b) } = 0 :- p.",
                ["f/1", "a/0"],
                "#sum { a(X): not p(f(X)): p(b) } = 0 :- p.",
                [set()]
                )
        self.assertEqual(str(cm.exception), "Evaluable functions are not allowed in negated literals in aggregates and body condition. Found f(X) at <string>:1:20-24.")

    def test_negative_predicate_in_aggregate_with_evaluable_head_aggregate_2(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualUnnesting(
                "#sum { a(X): p(f(X)): not p(a) } = 0 :- p.",
                ["f/1", "a/0"],
                "#sum { a(X): p(FUN): not p(a) } = 0 :- p.",
                [{"f(X)=FUN"}]
                )
        self.assertEqual(str(cm.exception), "Evaluable functions are not allowed in negated literals in aggregates and body condition. Found a at <string>:1:29-31.")

    def test_body_conditional_literal(self):
        self.assertEqualUnnesting(
            "p :- q(a): r(q(X)).",
            ["q/1", "a/0"],
            "p :- q(FUN): r(FUN2).",
            [{"a=FUN", "q(X)=FUN2"}]
            )

    def test_negative_predicate_in_body_conditional_literal(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualUnnesting(
                "p :- r(q(a)): not r(a).",
                ["q/1", "a/0"],
                "p :- q(FUN): not r(FUN2).",
                [{"q(a)=FUN", "a=FUN2"}]
                )
        self.assertEqual(str(cm.exception), "Evaluable functions are not allowed in negated literals in aggregates and body condition. Found a at <string>:1:21-23.")
         
    def test_aggregate_with_guard(self):
        self.assertEqualUnnesting(
            "p :- #sum { X: q(a) } = b; q(c).",
            ["a/0", "b/0", "c/0"],
            "p :- #sum { X: q(FUN) } = FUN2; q(FUN3).",
            [{"a=FUN", "b=FUN2", "c=FUN3"}],
        )
    #     # "p :- #sum { X: q(FUN), a=FUN } = FUN2; q(FUN3); b=FUN2; c=FUN3.
