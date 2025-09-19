import textwrap
import unittest

from clingo import ast
from clingo.core import Library
from fasp.util.ast import  StatementAST, FreshVariableGenerator

from fasp.ast.transformers.unnest_functions import UnnestFunctionsTransformer
from fasp.ast.syntax_checking import SymbolSignature

def rewrite_rule_with_unnested(transformer: UnnestFunctionsTransformer, st: StatementAST):
    """
    Helper to run transformer.transform_rule on a single statement while
    resetting the transformer's per-rule state (mimics previous method on transformer).
    Returns (new_statement, list_of_unnested_comparisons).
    """
    # reset per-rule state
    transformer.unnested_functions = []
    transformer._cache = {}
    transformer.var_gen = FreshVariableGenerator(set())

    new_st = transformer.transform_rule(st)
    return new_st, list(transformer.unnested_functions)


def rewrite_rules_with_unnested(transformer: UnnestFunctionsTransformer, rules):
    """
    Run rewrite_rule_with_unnested over a list of statements (rules).
    Returns list[(new_stmt, unnested_comps)].
    """
    out = []
    for r in rules:
        out.append(rewrite_rule_with_unnested(transformer, r))
    return out
class TestUnnestFunctionsTransformer(unittest.TestCase):

    def setUp(self):
        self.lib = Library()

    def parse_program(self, program: str):
        stmts = []
        ast.parse_string(self.lib, program, stmts.append)
        return stmts
    
    def apply_transform(self, program: str, evaluable_functions):
        """
        Utility: parse program, take rules (skip #program directive if present),
        and call rewrite_rules_with_unnested returning list[(stmt, comps)].
        """
        statements = self.parse_program(program)
        # skip the first statement if it is a #program directive
        rules = statements[1:] if len(statements) > 0 and isinstance(statements[0], ast.StatementProgram) else statements
        transformer = UnnestFunctionsTransformer(self.lib, evaluable_functions, set())
        return rewrite_rules_with_unnested(transformer, rules)

    def construct_new_program(self, rewritten):
        unnested_sets = []
        lines = ["#program base."]
        for rule_ast, comps in rewritten:
            lines.append(str(rule_ast).strip())
            # build a list of sets of unnested comparisons (one set per rule)
            unnested_sets.append(set(str(c).replace(" ", "") for c in comps))
        return "\n".join(lines).strip(), unnested_sets
    
    def test_unnest_example(self):
        program = textwrap.dedent("""\
            p(g(h(a,b),c),d(X)) :- q(X).
            q(X) :- g(1,a) = X.
            g(X,Y) = Z :- true.
        """).strip()

        expected_program = textwrap.dedent("""\
            #program base.
            p(FUN3,d(X)) :- q(X).
            q(X) :- g(1,FUN)=X.
            g(X,Y)=Z :- true.""").strip()
        
        
        evaluable_functions = {
            SymbolSignature("g", 2),
            SymbolSignature("h", 2),
            SymbolSignature("a", 0),
        }

        rewritten = self.apply_transform(program, evaluable_functions)

        new_program, unnested_sets = self.construct_new_program(rewritten)

        # expected sets:
        expected_set_first_rule = {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"}
        expected_set_second_rule = {"a=FUN"}  # second rule should not unnest outer g(1,a) but 'a' is an argument and should be unnested

        # Assertions: both expected sets should appear among per-rule unnested sets
        self.assertIn(expected_set_first_rule, unnested_sets)
        self.assertIn(expected_set_second_rule, unnested_sets)

        
        self.assertEqual(new_program, expected_program)
        

    def test_non_evaluable_symbolic_and_function(self):
        program = "q(a,b)."

        expected_program = textwrap.dedent("""
            #program base.
            q(a,b).""").strip()
        
        evaluable_functions = set()

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [set()])
    
    def test_tuple(self):
        program = "r((a,b))."

        expected_program = textwrap.dedent("""
            #program base.
            r((a,b)).
        """).strip()
        
        evaluable_functions = set()

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [set()])

    def test_absolute_unary_binary_operations(self):
        program = "s(abs(-1), -(1), 1+2)."

        expected_program = textwrap.dedent("""
            #program base.
            s(abs(-1),-1,1+2).
        """).strip()
        evaluable_functions = set()

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [set()])

    def test_absolute(self):
        program = "p(|f(a)|)."

        expected_program = textwrap.dedent("""
            #program base.
            p(|FUN|).
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertIn({"f(a)=FUN"}, unnested_sets)

    def test_absolute_with_evaluable_function(self):
        program = "s(abs(f(a)))."
        
        expected_program = textwrap.dedent("""
            #program base.
            s(abs(FUN)).
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertIn({"f(a)=FUN"}, unnested_sets)

    def test_non_rule_statement_rewrite(self):
        program = "#const n=10. [default]"
        statements = self.parse_program(program)

        # #const produces a StatementDefinition, not a StatementRule
        stmt = statements[0]

        transformer = UnnestFunctionsTransformer(self.lib, set(), set())
        new_stmt, comps = rewrite_rule_with_unnested(transformer, stmt)

        # Should be unchanged and still not a rule
        self.assertIsInstance(new_stmt, ast.StatementConst)
        self.assertEqual(str(new_stmt).strip(), "#const n=10. [default]")
        self.assertEqual(comps, [])

    def test_multiple_rules_and_variable_reuse(self):
        program = textwrap.dedent("""\
            p(f(a)).
            q(f(b)).
        """).strip()

        excepted_program = textwrap.dedent("""\
            #program base.
            p(FUN).
            q(FUN).
        """).strip()
        evaluable_functions = {SymbolSignature("f", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        self.assertEqual(len(rewritten), 2)

        new_program, unnested_sets = self.construct_new_program(rewritten)


        self.assertEqual(new_program, excepted_program) 
        # Each rule should introduce its own fresh FUN variable
        self.assertIn({'f(a)=FUN'}, unnested_sets)
        self.assertIn({'f(b)=FUN'}, unnested_sets)

    def test_nested_functions_chain(self):
        program = "r(g(h(f(a))))."
        evaluable_functions = {
            SymbolSignature("f", 1),
            SymbolSignature("h", 1),
            SymbolSignature("g", 1),
        }

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertIn("r(FUN", new_program)
        flat_comps = set().union(*unnested_sets)
        self.assertIn("f(a)=FUN", flat_comps)
        self.assertTrue(any("h(FUN" in c for c in flat_comps))
        self.assertTrue(any("g(" in c and "=FUN" in c for c in flat_comps))

    def test_function_in_head_not_unnested(self):
        program = "f(a) :- q."
        
        expected_program = textwrap.dedent("""
            #program base.
            f(a) :- q.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [set()])
    
    def test_mixed_head_and_body(self):
        program = "p(f(a)) :- q(f(b))."
        evaluable_functions = {SymbolSignature("f", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertIn("p(FUN)", new_program)
        self.assertIn("q(FUN2)", new_program)
        self.assertIn({"f(b)=FUN2", "f(a)=FUN"}, unnested_sets)
    
    def test_no_evaluable_functions(self):
        program = "r(f(a))."
        
        expected_program = textwrap.dedent("""
            #program base.
            r(f(a)).
        """).strip()
        
        evaluable_functions = set()

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [set()])

    def test_double_occurrence_same_function(self):
        program = textwrap.dedent("""
            s(f(a), f(a)).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            s(FUN,FUN).
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(len(unnested_sets), 1)
        self.assertIn({"f(a)=FUN"}, unnested_sets)
    
    def test_double_occurrence_same_function_and_symbolic_function(self):
        program = textwrap.dedent("""
            s(f(a), f(a)).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            s(FUN2,FUN2).
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("a", 0)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(len(unnested_sets), 1)
        self.assertIn({"f(FUN)=FUN2", "a=FUN"}, unnested_sets)

    def test_comparison(self):
        program = textwrap.dedent("""
            :- f(a) < f(b).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            :- FUN2<FUN3.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("a", 0)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(len(unnested_sets), 1)
        self.assertIn({"a=FUN", "f(FUN)=FUN2", "f(b)=FUN3"}, unnested_sets)


    def test_aggregate(self):
        program = textwrap.dedent("""
            :- #sum { a(X): p(f(X))} = 0.
            #sum { a(X): p(f(X))} = 0 :- p.
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            :- #sum { FUN: p(FUN2) } = 0.
            #sum { FUN: p(FUN2) } = 0 :- p.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("a", 1)}

        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        print(unnested_sets)
        self.assertEqual(new_program, expected_program)
        self.assertEqual(len(unnested_sets), 2)
        self.assertIn({"f(X)=FUN2","a(X)=FUN"}, unnested_sets)