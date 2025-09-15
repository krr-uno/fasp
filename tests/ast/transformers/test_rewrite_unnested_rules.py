import textwrap
import unittest

from clingo import ast
from clingo.core import Library
from fasp.util.ast import StatementAST
from fasp.ast.transformers.unnest_functions import UnnestFunctionsTransformer
from fasp.ast.syntax_checking import SymbolSignature


class TestUnnestFunctionsTransformer(unittest.TestCase):

    def setUp(self):
        self.lib = Library()

    def parse_program(self, program: str):
        stmts = []
        ast.parse_string(self.lib, program, stmts.append)
        return stmts
    
    def apply_transform(self, program: str, evaluable_functions):
        statements = self.parse_program(program)
        rules = statements[1:] if len(statements) > 0 and isinstance(statements[0], ast.StatementProgram) else statements
        transformer = UnnestFunctionsTransformer(self.lib, evaluable_functions, set())
        return transformer.rewrite_rules_with_unnested(rules)

    def construct_new_program(self, rewritten):
        unnested_sets = []
        lines = ["#program base."]
        for rule_ast, comps in rewritten:
            lines.append(str(rule_ast).strip())
            unnested_sets.append(set(str(c).replace(" ", "") for c in comps))
        return "\n".join(lines).strip(), unnested_sets
    
    def test_unnest_example(self):
        program = textwrap.dedent("""
            p(g(h(a,b),c),d(X)) :- q(X).
            q(X) :- g(1,a) = X.
            g(X,Y) = Z :- true.
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            p(FUN3,d(X)) :- q(X); a=FUN; h(FUN,b)=FUN2; g(FUN2,c)=FUN3.
            q(X) :- g(1,FUN)=X; a=FUN.
            g(X,Y)=Z :- true.
        """).strip()
        
        evaluable_functions = {
            SymbolSignature("g", 2),
            SymbolSignature("h", 2),
            SymbolSignature("a", 0),
        }
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        expected_set_first_rule = {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"}
        expected_set_second_rule = {"a=FUN"}

        self.assertIn(expected_set_first_rule, unnested_sets)
        self.assertIn(expected_set_second_rule, unnested_sets)
        self.assertIn("p(FUN3,d(X)) :- q(X); a=FUN; h(FUN,b)=FUN2; g(FUN2,c)=FUN3.", new_program)
        self.assertEqual(new_program, expected_program)
        

    def test_non_evaluable_symbolic_and_function(self):
        program = textwrap.dedent("""
            q(a,b).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            q(a,b).
        """).strip()
        
        evaluable_functions = set()
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(unnested_sets, [set()])
    
    def test_tuple(self):
        program = textwrap.dedent("""
            r((a,b)).
        """).strip()

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
        program = textwrap.dedent("""
            s(abs(-1), -(1), 1+2).
        """).strip()

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
        program = textwrap.dedent("""
            p(|f(a)|).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            p(|FUN|) :- f(a)=FUN.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertIn({"f(a)=FUN"}, unnested_sets)

    def test_absolute_with_evaluable_function(self):
        program = textwrap.dedent("""
            s(abs(f(a))).
        """).strip()
        
        expected_program = textwrap.dedent("""
            #program base.
            s(abs(FUN)) :- f(a)=FUN.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertIn({"f(a)=FUN"}, unnested_sets)

    def test_non_rule_statement_rewrite(self):
        program = textwrap.dedent("""
            #const n=10. [default]
        """).strip()
        statements = self.parse_program(program)
        stmt = statements[0]

        transformer = UnnestFunctionsTransformer(self.lib, set(), set())
        new_stmt, comps = transformer.rewrite_rule_with_unnested(stmt)

        self.assertIsInstance(new_stmt, ast.StatementConst)
        self.assertEqual(str(new_stmt).strip(), "#const n=10. [default]")
        self.assertEqual(comps, [])

    def test_multiple_rules_and_variable_reuse(self):
        program = textwrap.dedent("""
            p(f(a)).
            q(f(b)).
        """).strip()

        expected_program = textwrap.dedent("""
            #program base.
            p(FUN) :- f(a)=FUN.
            q(FUN) :- f(b)=FUN.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program) 
        self.assertIn({'f(a)=FUN'}, unnested_sets)
        self.assertIn({'f(b)=FUN'}, unnested_sets)

    def test_nested_functions_chain(self):
        program = textwrap.dedent("""
            r(g(h(f(a)))).
        """).strip()

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
        program = textwrap.dedent("""
            f(a) :- q.
        """).strip()
        
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
        program = textwrap.dedent("""
            p(f(a)) :- q(f(b)).
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertIn("p(FUN)", new_program)
        self.assertIn("q(FUN2)", new_program)
        self.assertIn({"f(b)=FUN2", "f(a)=FUN"}, unnested_sets)
    
    def test_no_evaluable_functions(self):
        program = textwrap.dedent("""
            r(f(a)).
        """).strip()
        
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
            s(FUN,FUN) :- f(a)=FUN.
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
            s(FUN2,FUN2) :- a=FUN; f(FUN)=FUN2.
        """).strip()
        
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("a", 0)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expected_program)
        self.assertEqual(len(unnested_sets), 1)
        self.assertIn({"f(FUN)=FUN2", "a=FUN"}, unnested_sets)
