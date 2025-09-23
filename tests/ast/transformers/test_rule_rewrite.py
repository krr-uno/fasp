
import textwrap
import unittest
from clingo import ast
from clingo.core import Library
from fasp.util.ast import collect_variables, collect_comparisons
from fasp.ast.transformers.rule_rewrite import RuleRewriteTransformer
from fasp.ast.syntax_checking import SymbolSignature

class TestRuleRewriteTransformer(unittest.TestCase):
    def setUp(self):
        self.lib = Library()

    def parse_program(self, program: str):
        stmts = []
        ast.parse_string(self.lib, program, stmts.append)
        return stmts

    def test_basic_unnest(self):
        program = textwrap.dedent("""
            p(f(1),a) :- q(g(1),b), not not r(h(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p(FUN,a) :- q(FUN2,b); g(1)=FUN2; not not r(FUN3); h(1)=FUN3; f(1)=FUN.
        """).strip()
        evaluable_functions = {
            SymbolSignature("f", 1),
            SymbolSignature("g", 1),
            SymbolSignature("h", 1),
        }
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        # Build the new program string
        lines = ["#program base.", str(new_rule).strip()]
        new_program = "\n".join(lines).strip()
        # Collect unnested comparisons as strings (for demonstration)
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        # Check rewritten program
        self.assertEqual(new_program, expected_program)
        # Check unnested comparisons
        self.assertEqual(unnested_set, {"f(1)=FUN", "g(1)=FUN2", "h(1)=FUN3"})

    
    def test_basic_unnest2(self):
        program = textwrap.dedent("""
            p(f(1),a) :- q(g(1),b), not not r(h(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p(f(1),a) :- q(g(1),b); not not r(h(1)).
        """).strip()
        evaluable_functions = {}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        # Build the new program string
        lines = ["#program base.", str(new_rule).strip()]
        new_program = "\n".join(lines).strip()
        # Collect unnested comparisons as strings (for demonstration)
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        # Check rewritten program
        self.assertEqual(new_program, expected_program)
        
    def test_no_evaluable_functions(self):
        """Test that no transformation occurs when no functions are evaluable."""
        program = textwrap.dedent("""
            p(f(1),a) :- q(g(1),b), not not r(h(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p(f(1),a) :- q(g(1),b); not not r(h(1)).
        """).strip()
        evaluable_functions = set()
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(len(unnested_set), 0)

    def test_head_function_only(self):
        """Test unnested function only in head."""
        program = textwrap.dedent("""
            f(c) :- d.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(FUN) :- d; c=FUN.
        """).strip()
        evaluable_functions = {SymbolSignature("c", 0)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"c=FUN"})

    def test_double_negation_body(self):
        """Test a function under double negation in the body."""
        program = textwrap.dedent("""
            p :- not not q(f(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p :- not not q(FUN); f(1)=FUN.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(1)=FUN"})

    def test_body_positive_literal(self):
        """Test a positive literal in the body with evaluable function."""
        program = textwrap.dedent("""
            p :- q(f(1)).
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            p :- q(FUN); f(1)=FUN.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(1)=FUN"})

    def test_body_aggregate_element(self):
        """Test that aggregate elements are rewritten and comparisons appended."""
        program = textwrap.dedent("""
            f(X) :- W = #sum { f(Y) : p(g(Y)), q(X) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(X) :- W = #sum { FUN: p(FUN2), q(X), g(Y)=FUN2, f(Y)=FUN }.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(Y)=FUN", "g(Y)=FUN2"})

    def test_body_aggregate(self):
        """Test that an aggregate in the body is rewritten correctly."""
        program = textwrap.dedent("""
            f(X) :- #sum { p(f(Y)), q(X) } = W.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(X) :- #sum { p(FUN),q(X): f(Y)=FUN } = W.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(Y)=FUN"})

    def test_negative_body_literal_creates_conditional_literal(self):
        program = "p :- not q(f(1))."
        evaluable_functions = {SymbolSignature("f", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)

        body = new_rule.body
        conds = [b for b in body if isinstance(b, ast.BodyConditionalLiteral)]
        self.assertTrue(any(conds), "Expected BodyConditionalLiteral in body")

        comps = collect_comparisons(new_rule)
        self.assertTrue(any(isinstance(c, ast.LiteralComparison) for c in comps))


    def test_body_aggregate_and_head(self):
        program = textwrap.dedent("""
            f(X) = W :- b(X,Z), W = #sum { f(Y) : p(g(Y),Z), q(X), r(X) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            f(X)=W :- b(X,Z); W = #sum { FUN: p(FUN2,Z), q(X), r(X), g(Y)=FUN2, f(Y)=FUN }.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(Y)=FUN", "g(Y)=FUN2"})

    def test_body_aggregate_element_list_branch(self):
        """Trigger the list branch in BodyAggregateElement rewrite."""
        program = textwrap.dedent("""
            a :- #sum { q(f(1)), r(f(2)) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            a :- #sum { q(FUN),r(FUN2): f(1)=FUN, f(2)=FUN2 }.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(1)=FUN", "f(2)=FUN2"})

    def test_body_aggregate_element_list_branch_for_coverage(self):
        """Force BodyAggregateElement list branch to execute for coverage."""
        program = textwrap.dedent("""
            a :- #sum { q(f(1)) }.
        """).strip()
        expected_program = textwrap.dedent("""
            #program base.
            a :- #sum { q(FUN): f(1)=FUN }.
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1)}
        stmts = self.parse_program(program)
        rule = stmts[1]
        transformer = RuleRewriteTransformer(self.lib, evaluable_functions)
        new_rule = transformer.transform_rule(rule)
        new_program = "#program base.\n" + str(new_rule).strip()
        unnested_set = set(str(c).replace(" ", "") for c in transformer.unnest_transformer.unnested_functions)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_set, {"f(1)=FUN"})

