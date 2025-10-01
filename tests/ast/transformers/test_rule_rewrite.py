import textwrap
import unittest

from clingo import ast
from clingo.core import Library
from fasp.ast.syntax_checking import SymbolSignature
from fasp.ast.transformers.rule_rewrite import RuleRewriteTransformer


def apply_rule_rewrite(lib, program: str, evaluable_functions):
    """
    Parse program and apply RuleRewriteTransformer to each rule.
    Returns (new_program, [set_of_unnested_per_rule]).
    """
    stmts = []
    ast.parse_string(lib, program, stmts.append)

    # skip #program directive if present
    rules = stmts[1:] if len(stmts) > 0 and isinstance(stmts[0], ast.StatementProgram) else stmts

    rewritten_rules = []

    for rule in rules:
        transformer = RuleRewriteTransformer(lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        rewritten_rules.append(str(new_rule).strip())
    new_program = "\n".join(rewritten_rules).strip()
    return new_program


class TestRuleRewriteTransformer(unittest.TestCase):
    def setUp(self):
        self.lib = Library()

    def assertEqualRewrite(self, evaluable_functions: set[str], program:str, expected_program: str):
        evaluable_functions = [ s.split("/") for s in evaluable_functions]
        evaluable_functions = {SymbolSignature(n,int(a)) for n,a in evaluable_functions}
        program = textwrap.dedent(program).strip()
        expected_program = textwrap.dedent(expected_program).strip()
        new_program = apply_rule_rewrite(self.lib, program, evaluable_functions)
        self.assertEqual(new_program, expected_program)

    def test_basic_unnest(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "p(f(1),a) :- q(g(1),b), not not r(h(1)).",
            "p(FUN,a) :- q(FUN2,b); g(1)=FUN2; not not r(FUN3); h(1)=FUN3; f(1)=FUN."
        )
    
    def test_no_evaluable_functions(self):
        self.assertEqualRewrite(
            {},
            "p(f(1),a) :- q(g(1),b), not not r(h(1)).",
            "p(f(1),a) :- q(g(1),b); not not r(h(1))."
        )
        
    def test_head_function_only(self):
        self.assertEqualRewrite(
            {"c/0"},
            "f(c) :- d.",
            "f(FUN) :- d; c=FUN."
        )

    def test_double_negation_body(self):
        self.assertEqualRewrite(
            {"f/1"},
            "p :- not not q(f(1)).",
            "p :- not not q(FUN); f(1)=FUN."
        )

    def test_body_positive_literal(self):
        self.assertEqualRewrite(
            {"f/1"},
            "p :- q(f(1)).",
            "p :- q(FUN); f(1)=FUN."
        )        

    def test_body_aggregate_element(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "f(X) :- W = #sum { f(Y) : p(g(Y)), q(X) }.",
            "f(X) :- W = #sum { FUN: p(FUN2), q(X), f(Y)=FUN, g(Y)=FUN2 }."
        )

    def test_body_aggregate(self):
        self.assertEqualRewrite(
            {"f/1"},
            "f(X) :- #sum { p(f(Y)), q(X) } = W.",
            "f(X) :- #sum { p(FUN),q(X): f(Y)=FUN } = W."
        )

    def test_negative_body_literal_creates_conditional_literal(self):
        self.assertEqualRewrite(
            {"f/1"},
            "p :- not q(f(1)), r.",
            "p :- #false: q(FUN), f(1)=FUN; r."
        )

    def test_body_aggregate_and_head(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "f(X)=W :- b(X,Z), W = #sum { f(Y): p(g(Y),Z), q(X), r(X) }.",
            "f(X)=W :- b(X,Z); W = #sum { FUN: p(FUN2,Z), q(X), r(X), f(Y)=FUN, g(Y)=FUN2 }."
        )
        
    def test_body_aggregate_element_list(self):
        self.assertEqualRewrite(
            {"f/1"},
            "a :- #sum { q(f(1)),r(f(2)) }.",
            "a :- #sum { q(FUN),r(FUN2): f(1)=FUN, f(2)=FUN2 }."
            )

    def test_body_aggregate_element_list_2(self):
        self.assertEqualRewrite(
            {"f/1"},
            "a :- #sum { q(f(1)) }.",
            "a :- #sum { q(FUN): f(1)=FUN }."
        )

    def test_head_function_rewrite_always_adds_comparison(self):
        self.assertEqualRewrite(
            {"f/1", "g/1"},
            "p(f(1),g(2)) :- q.",
            "p(FUN,FUN2) :- q; f(1)=FUN; g(2)=FUN2."
        )
    
    def test_aggregates(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(X)=W :- b(X,Z), W = #sum { f(Y): p(g(Y),Z), q(X), r(X) }.",
            "f(X)=W :- b(X,Z); W = #sum { FUN: p(FUN2,Z), q(X), r(X), f(Y)=FUN, g(Y)=FUN2 }."
        )

    def test_aggregates_2(self):
        self.assertEqualRewrite(
            {"f/1", "g/1", "h/1"},
            "f(X)=1 :- b(X,Z), h(1) = #sum { f(Y): p(g(Y),Z), q(X), r(X) }.",
            "f(X)=1 :- b(X,Z); FUN = #sum { FUN2: p(FUN3,Z), q(X), r(X), f(Y)=FUN2, g(Y)=FUN3 }; h(1)=FUN."
        )

    def test_negative_literal_in_aggregate_condition_raises(self):
        
        with self.assertRaises(RuntimeError) as cm:
            self.assertEqualRewrite(
                {"q/1"},
                "p :- #sum { X : not q(X) } = 1.",
                "p :- #sum { X : not q(X) } = 1."
            )

        # TODO: Need to modify transformer to only raise error when negative literal has evaluable function
        # Currently it raises error for all negative literals in aggregate condition.
        self.assertIn(
            "Negative literals in aggregate conditions are not supported",
            # Evaluable functions in negative literals in aggregate conditions are currently not supported
            str(cm.exception)
        )