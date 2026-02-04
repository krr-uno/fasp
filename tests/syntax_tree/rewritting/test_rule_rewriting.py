import textwrap
import unittest

from clingo import ast

from fasp.syntax_tree.parsing.parser import parse_string
from fasp.util.ast import ELibrary
from fasp.syntax_tree.collectors import SymbolSignature
from fasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer


class TestRuleRewriteTransformer(unittest.TestCase):
    def setUp(self):
        self.lib = ELibrary()

    def apply_rule_rewrite(self, program: str, evaluable_functions):
        """
        Parse program and apply RuleRewriteTransformer to each rule.
        Returns the rewritten program as a string.
        """
        stmts = parse_string(self.lib, program)

        # skip #program directive if present
        rules = (
            stmts[1:] if stmts and isinstance(stmts[0], ast.StatementProgram) else stmts
        )
        rewritten_rules = []
        for rule in rules:
            transformer = RuleRewriteTransformer(self.lib.library, evaluable_functions)
            new_rule = transformer.transform_rule(rule)
            rewritten_rules.append(str(new_rule).strip())

        return "\n".join(rewritten_rules).strip()

    def assertEqualRewrite(
        self, evaluable_functions: set[str], program: str, expected_program: str | None
    ):
        """
        Helper to check that a rewrite produces exactly the expected text.
        """
        evaluable_functions = {
            SymbolSignature(name, int(arity))
            for name, arity in (f.split("/") for f in evaluable_functions)
        }
        program = textwrap.dedent(program).strip()
        new_program = self.apply_rule_rewrite(program, evaluable_functions)

        if expected_program is not None:
            expected_program = textwrap.dedent(expected_program).strip()
            self.assertEqual(new_program, expected_program)

    # Tests
    def test_basic_unnest(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "p(f(1),a) :- q(g(1),b), r(h(1)).",
            "p(FUN,a) :- q(FUN2,b); r(FUN3); f(1)=FUN; g(1)=FUN2; h(1)=FUN3.",
        )

    def test_no_evaluable_functions(self):
        self.assertEqualRewrite(
            {},
            "p(f(1),a) :- q(g(1),b), not not r(h(1)).",
            "p(f(1),a) :- q(g(1),b); not not r(h(1)).",
        )

    def test_head_function_only(self):
        self.assertEqualRewrite({"c/0"}, "f(c) :- d.", "f(FUN) :- d; c=FUN.")

    def test_body_positive_literal(self):
        self.assertEqualRewrite({"f/1"}, "p :- q(f(1)).", "p :- q(FUN); f(1)=FUN.")

    def test_head_simple_assignment(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "x(X) := h(f(X))+g(Y).",
            "x(X) := h(FUN)+FUN2 :- f(X)=FUN; g(Y)=FUN2.",
        )

    def test_body_aggregate_element(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "f(X) :- W = #sum { f(Y) : p(g(Y)), q(X) }.",
            "f(X) :- W = #sum { FUN: p(FUN2), q(X), f(Y)=FUN, g(Y)=FUN2 }.",
        )

    def test_body_aggregate(self):
        self.assertEqualRewrite(
            {"f/1"},
            "f(X) :- #sum { p(f(Y)),q(X) } = W.",
            "f(X) :- #sum { p(FUN),q(X): f(Y)=FUN } = W.",
        )

    def test_negative_body_literal_creates_conditional_literal(self):
        self.assertEqualRewrite(
            {"f/1"}, "p :- not q(f(1)), r.", "p :- #false: q(FUN), f(1)=FUN; r."
        )

    def test_body_aggregate_element_list(self):
        self.assertEqualRewrite(
            {"f/1"},
            "a :- #sum { q(f(1)),r(f(2)) }.",
            "a :- #sum { q(FUN),r(FUN2): f(1)=FUN, f(2)=FUN2 }.",
        )

    def test_body_aggregate_element_list_2(self):
        self.assertEqualRewrite(
            {"f/1"}, "a :- #sum { q(f(1)) }.", "a :- #sum { q(FUN): f(1)=FUN }."
        )

    def test_head_function_rewrite_always_adds_comparison(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p(f(1),g(2)) :- q.",
            "p(FUN,FUN2) :- q; f(1)=FUN; g(2)=FUN2.",
        )

    def test_assignment_rule(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "f(X) := g(X).",
            "f(X) := FUN :- g(X)=FUN.",
        )

    def test_assignment_rule_2(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "f(X) := g(X) :- p(f(X)).",
            "f(X) := FUN :- p(FUN2); g(X)=FUN; f(X)=FUN2.",
        )

    def test_function_and_predicate(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            ":- f(X)=g(X); f(X).",
            ":- f(X)=FUN; f(X); g(X)=FUN.",
        )

    def test_comparisons(self):
        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- f(c) = a.",
            ":- f(c)=FUN; a=FUN.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0"},
            ":- f(a)<f(b).",
            ":- FUN2<FUN3; a=FUN; f(FUN)=FUN2; f(b)=FUN3.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- f(a)=f(b).",
            ":- f(FUN)=FUN2; a=FUN; f(b)=FUN2.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- f(b)<g(x).",
            ":- FUN<g(x); f(b)=FUN.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- f(c)!=d(c).",
            ":- FUN!=d(c); f(c)=FUN.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- f(c) < a.",
            ":- FUN<FUN2; f(c)=FUN; a=FUN2.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- g = a.",
            ":- a=g.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "h/1"},
            ":- g = f(1).",
            ":- f(1)=g.",
        )

        self.assertEqualRewrite(
            {"a/0", "b/0"},
            "a>b.",
            "FUN>FUN2 :- a=FUN; b=FUN2.",
        )

    def test_comparison_with_equality_in_head(self):
        self.assertEqualRewrite(
            {"a/0", "b/0"},
            "a=b.",
            "FUN=FUN2 :- a=FUN; b=FUN2.",
        )

    def test_head_aggregate_simple(self):
        self.assertEqualRewrite(
            {"f/1"},
            "#sum { a(X): p: p(f(X)) } = 0 :- p.",
            "#sum { a(X): p: p(FUN), f(X)=FUN } = 0 :- p.",
        )

    def test_head_aggregate_with_literal(self):
        self.assertEqualRewrite(
            {"f/1"},
            "#sum { a(X): p(f(X)): p(a) } = 0 :- p.",
            "#sum { a(X): p(FUN): p(a), f(X)=FUN } = 0 :- p.",
        )

    def test_head_aggregate_guard(self):
        self.assertEqualRewrite(
            {"f/1"},
            "#sum { a(X): p(f(X)): p(a) } = f(2) :- p.",
            "#sum { a(X): p(FUN): p(a), f(X)=FUN } = FUN2 :- p; f(2)=FUN2.",
        )

    def test_arithmetic_in_body(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p :- f(a)-g(b)=10.",
            "p :- FUN-FUN2=10; f(a)=FUN; g(b)=FUN2.",
        )

        self.assertEqualRewrite(
            {"f/1"},
            "p :- 10=f(a)-h(b).",
            "p :- 10=FUN-h(b); f(a)=FUN.",
        )

        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p :- f(a)*g(b)=f(X).",
            "p :- f(X)=FUN*FUN2; f(a)=FUN; g(b)=FUN2.",
        )

        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p :- h(X)=f(a)*g(b).",
            "p :- h(X)=FUN*FUN2; f(a)=FUN; g(b)=FUN2.",
        )

        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p :- h(X)=f(a)/g(b).",
            "p :- h(X)=FUN/FUN2; f(a)=FUN; g(b)=FUN2.",
        )

        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "x(X) := h(f(X))-g(Y).",
            "x(X) := h(FUN)-FUN2 :- f(X)=FUN; g(Y)=FUN2.",
        )

    def test_double_negation_body(self):
        self.assertEqualRewrite(
            {"f/1"}, "p :- not not q(f(1)).", "p :- not not q(FUN); not not f(1)=FUN."
        )

        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "p(f(1),a) :- q(g(1),b), not not r(h(1)).",
            "p(FUN,a) :- q(FUN2,b); not not r(FUN3); f(1)=FUN; g(1)=FUN2; not not h(1)=FUN3.",
        )

    def test_double_negation_head(self):
        self.assertEqualRewrite(
            {"f/1"}, "not not p(f(1)) :- q.", "not not p(FUN) :- q; not not f(1)=FUN."
        )

    def test_head_aggregate_assignment(self):
        with self.assertRaises(AssertionError) as cm:
            self.assertEqualRewrite(
                {"f/1", "p/1"},
                "score(X) := #sum{f(Y): f(p(Y)), q(X) } :- p.",
                "score(X) := #sum{f(Y): f(FUN), q(X)} :- p; p(Y)=FUN.",
                # "score(X) := #sum{f(Y): f(FUN), q(X), p(Y)=FUN} :- p.",
            )

        self.assertEqual(
            str(cm.exception),
            "HeadAggregateAssignment seen during function unnesting. This should not happen.",
        )

    def test_body_aggregate_with_guard(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p :- #sum { g(X): q(X), h(X)} = f(2).",
            "p :- #sum { FUN: q(X), h(X), g(X)=FUN } = FUN2; f(2)=FUN2.",
        )

    def test_aggregate(self):
        self.assertEqualRewrite(
            {"f/1"},
            ":- W = #sum { X: p(f(Y),X) }.",
            ":- W = #sum { X: p(FUN,X), f(Y)=FUN }.",
        )

    def test_aggregate_guard(self):
        self.assertEqualRewrite(
            {"f/1"},
            ":- f(Y) = #sum { X: p(X) }.",
            ":- FUN = #sum { X: p(X) }; f(Y)=FUN.",
        )

    def test_aggregates(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(X)=W :- b(X,Z), W = #sum { f(Y): p(g(Y),Z), q(X), r(X) }.",
            "FUN=W :- b(X,Z); W = #sum { FUN2: p(FUN3,Z), q(X), r(X), f(Y)=FUN2, g(Y)=FUN3 }; f(X)=FUN.",
        )

    def test_aggregates_2(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            ":- b(X,Z), h(1) = #sum { f(Y): p(g(Y),Z), q(X), r(X) }.",
            ":- b(X,Z); FUN3 = #sum { FUN: p(FUN2,Z), q(X), r(X), f(Y)=FUN, g(Y)=FUN2 }; h(1)=FUN3.",
        )

    def test_assignment_simple(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(a) := 5.",
            "f(a) := 5.",
        )

    def test_assignment(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(g(a)) := h(a).",
            "f(FUN) := FUN2 :- g(a)=FUN; h(a)=FUN2.",
        )

    def test_assignment_body(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(g(a)) := h(a) :- f(g(b)).",
            "f(FUN) := FUN2 :- f(FUN3); g(a)=FUN; h(a)=FUN2; g(b)=FUN3.",
        )

    def test_aggregates_2(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(X)=1 :- b(X,Z), h(1) = #sum { f(Y): p(g(Y),Z), q(X), r(X) }.",
            "FUN=1 :- b(X,Z); FUN4 = #sum { FUN2: p(FUN3,Z), q(X), r(X), f(Y)=FUN2, g(Y)=FUN3 }; f(X)=FUN; h(1)=FUN4.",
        )

    def test_negative_predicate_in_aggregate_with_evaluable_body_aggregate(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualRewrite(
                {"q/1", "a/0"},
                "p :- #sum { X: q(a), not r(q(X)) } = 1.",
                "p :- #sum { X: q(FUN), r(FUN2), a=FUN, q(X)=FUN2 } = 1.",
            )
        self.assertEqual(
            str(cm.exception),
            "Evaluable functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found 'q(X)' at <string>:1:28-32.",
        )

    def test_negative_predicate_in_aggregate_without_evaluable_body_aggregte(self):
        self.assertEqualRewrite(
            {"q/1", "a/0"},
            "p :- #sum { X: not q(b), r(q(X)) } = 1.",
            "p :- #sum { X: not q(b), r(FUN), q(X)=FUN } = 1.",
        )

    def test_negative_predicate_in_aggregate_with_evaluable_head_aggregate(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualRewrite(
                {"f/1", "a/0"},
                "#sum { a(X): not p(f(X)): not p(b) } = 0 :- p.",
                "#sum { a(X): not p(f(X)): not p(b) } = 0 :- p.",
            )
        self.assertEqual(
            str(cm.exception),
            "Evaluable functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found 'f(X)' at <string>:1:20-24.",
        )

    def test_body_conditional_literal(self):
        self.assertEqualRewrite(
            {"q/1", "a/0"},
            "p :- q(a): r(q(X)).",
            "p :- q(FUN): r(FUN2), q(X)=FUN2; a=FUN.",
        )

    def test_negative_predicate_in_body_conditional_literal(self):
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualRewrite({"q/1", "a/0"}, "p :- q(a): not r(q(X)).", None)
        self.assertEqual(
            str(cm.exception),
            "Evaluable functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found 'q(X)' at <string>:1:18-22.",
        )

    def test_aggregate_with_guard(self):
        self.assertEqualRewrite(
            {"a/0", "b/0", "c/0"},
            "p :- #sum { X: q(a) } = b; q(c).",
            "p :- #sum { X: q(FUN), a=FUN } = FUN2; q(FUN3); b=FUN2; c=FUN3.",
        )

    def test_body_negative_literal_no_evaluable_functions(self):
        self.assertEqualRewrite(
            set(), "p :- not p(q).", "p :- not p(q)."  # no evaluable functions
        )

    def test_head_aggregate_with_evaluable_along_with_guard(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "#sum { f(Y) : p(f(2)) : p(g(Y),Z), q(X), r(X) } >= f(Z).",
            "#sum { FUN: p(FUN3): p(FUN2,Z), q(X), r(X), f(Y)=FUN, g(Y)=FUN2, f(2)=FUN3 } >= FUN4 :- f(Z)=FUN4.",
        )

    def test_head_aggregate_with_assignment(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "#count{ 0,ass(king(C),X): king(C) := X: person(X) } :- country(C).",
            "#count{ 0,ass(king(C),X): king(C) := X: person(X) } :- country(C).",
        )

    def test_head_aggregate_with_assignment2(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1", "e/1"},
            "#count{ 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)) } :- country(C).",
            "#count{ 0,ass(king(FUN),X): king(FUN3) := FUN4: person(FUN2), f(C)=FUN, e(X)=FUN2, g(C)=FUN3, h(X)=FUN4 } :- country(C).",
        )

    def test_fibo(self):
        self.assertEqualRewrite(
            {"fibo/1"},
            "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
            "fibo(X) := Y :- number(X); X>1; FUN+FUN2=Y; fibo(X-1)=FUN; fibo(X-2)=FUN2.",
        )

    # # TODO: Fix for such case.
    # def test_body_conditional_literal_false(self):
    #      self.assertEqualRewrite(
    #         set(),
    #         "a :- #false: c.",
    #         "a :- #false: c."
    #     )
    def test_body_conditional_literal_false(self):
        self.assertEqualRewrite(
            {"f/1"},
            """
            f(X) := 1 :- p(X).
            a :- p(C); #false: country(f(b)).
            """,
            """
            f(X) := 1 :- p(X).
            a :- p(C); #false: country(FUN), f(b)=FUN.
            """,
        )

    def test_no_rewrite_head_set_aggregate(self):
        self.assertEqualRewrite(
            {"q/1", "a/0"},
            ":- { X=country(f(b)) }.",
            ":- { X=country(f(b)) }.",
        )

    def test_statement_optimize(self):
        self.assertEqualRewrite(
            {"f/1", "a/0"},
            "#minimize { 1@0,f(X),a: p(X) }.",
            "#minimize { 1@0,FUN,FUN2: p(X), f(X)=FUN, a=FUN2 }.",
        )

        self.assertEqualRewrite(
            {"f/1", "a/0", "b/0"},
            "#maximize { f(X),a: p(X),f(b) }.",
            "#maximize { FUN,FUN2: p(X), f(FUN3), f(X)=FUN, a=FUN2, b=FUN3 }.",
        )
    def test_weak_constraint(self):
        self.assertEqualRewrite(
            {"f/1","a/0"},
            ":~ p(X). [f(a)]",
            ":~ p(X); a=FUN; f(FUN)=FUN2. [FUN2]",
        )
