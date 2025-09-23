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
    unnested_sets = []

    for rule in rules:
        transformer = RuleRewriteTransformer(lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        rewritten_rules.append(str(new_rule).strip())
        unnested_sets.append(
            set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        )

    new_program = "#program base.\n" + "\n".join(rewritten_rules).strip()
    return new_program, unnested_sets


class TestRuleRewriteTransformer(unittest.TestCase):
    def setUp(self):
        self.lib = Library()

    def test_basic_unnest(self):
        program = textwrap.dedent("""
            p(f(1),a) :- q(g(1),b), not not r(h(1)).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            p(FUN,a) :- q(FUN2,b); g(1)=FUN2; not not r(FUN3); h(1)=FUN3; f(1)=FUN.
        """).strip()

        evaluable = {
            SymbolSignature("f", 1), 
            SymbolSignature("g", 1), 
            SymbolSignature("h", 1)
        }

        new_program, unnested_sets = apply_rule_rewrite(self.lib, textwrap.dedent(program).strip(), evaluable)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(1)=FUN", "g(1)=FUN2", "h(1)=FUN3"})

    def test_no_evaluable_functions(self):
        program = textwrap.dedent("""
            p(f(1),a) :- q(g(1),b), not not r(h(1)).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            p(f(1),a) :- q(g(1),b); not not r(h(1)).
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, set())

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], set())

    def test_head_function_only(self):
        program = textwrap.dedent("""
            f(c) :- d.
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            f(FUN) :- d; c=FUN.
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("c", 0)})

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"c=FUN"})

    def test_double_negation_body(self):
        program = textwrap.dedent("""
            p :- not not q(f(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p :- not not q(FUN); f(1)=FUN.
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("f", 1)})

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(1)=FUN"})

    def test_body_positive_literal(self):
        program = textwrap.dedent("""
            p :- q(f(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p :- q(FUN); f(1)=FUN.
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("f", 1)})

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(1)=FUN"})

    def test_body_aggregate_element(self):
        program = textwrap.dedent("""
            f(X) :- W = #sum { f(Y) : p(g(Y)), q(X) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(X) :- W = #sum { FUN: p(FUN2), q(X), g(Y)=FUN2, f(Y)=FUN }.
        """).strip()

        evaluable = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, evaluable)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(Y)=FUN", "g(Y)=FUN2"})

    def test_body_aggregate(self):
        program = textwrap.dedent("""
            f(X) :- #sum { p(f(Y)), q(X) } = W.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(X) :- #sum { p(FUN),q(X): f(Y)=FUN } = W.
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("f", 1)})

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(Y)=FUN"})

    def test_negative_body_literal_creates_conditional_literal(self):
        program = textwrap.dedent("""
            p :- not q(f(1)), r.
        """).strip()
        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("f", 1)})
        expected_program = textwrap.dedent("""
            #program base.
            p :- #false: q(FUN), f(1)=FUN; r.
        """).strip()       
        self.assertEqual(new_program, expected_program) 

        # ensure comparisons are present
        self.assertIn("f(1)=FUN", unnested_sets[0])

    def test_body_aggregate_and_head(self):
        program = textwrap.dedent("""
            f(X) = W :- b(X,Z), W = #sum { f(Y) : p(g(Y),Z), q(X), r(X) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(X)=W :- b(X,Z); W = #sum { FUN: p(FUN2,Z), q(X), r(X), g(Y)=FUN2, f(Y)=FUN }.
        """).strip()

        evaluable = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, evaluable)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(Y)=FUN", "g(Y)=FUN2"})

    def test_body_aggregate_element_list_branch(self):
        program = textwrap.dedent("""
            a :- #sum { q(f(1)), r(f(2)) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            a :- #sum { q(FUN),r(FUN2): f(1)=FUN, f(2)=FUN2 }.
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("f", 1)})

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(1)=FUN", "f(2)=FUN2"})

    def test_body_aggregate_element_list_branch_for_coverage(self):
        program = textwrap.dedent("""
            a :- #sum { q(f(1)) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            a :- #sum { q(FUN): f(1)=FUN }.
        """).strip()

        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, {SymbolSignature("f", 1)})

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(1)=FUN"})

    def test_head_function_rewrite_always_adds_comparison(self):
        program = textwrap.dedent("""
            p(f(1), g(2)) :- q.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p(FUN,FUN2) :- q; f(1)=FUN; g(2)=FUN2.
        """).strip()

        evaluable = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, evaluable)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets[0], {"f(1)=FUN", "g(2)=FUN2"})
    def test_aggregates(self):
        """Ensure head comparisons are added to the body even if already present."""
        program = textwrap.dedent("""
            f(X) = W :- b(X,Z), W = #sum{ f(Y) : p(g(Y),Z), q(X), r(X) }.
            
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            f(X)=W :- b(X,Z); W = #sum { FUN: p(FUN2,Z), q(X), r(X), g(Y)=FUN2, f(Y)=FUN }.
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1), SymbolSignature("h", 1)}
        new_program, unnested_sets = apply_rule_rewrite(self.lib, program, evaluable_functions)

        # Duplicates in body are expected now
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [{'f(Y)=FUN', 'g(Y)=FUN2'}])
