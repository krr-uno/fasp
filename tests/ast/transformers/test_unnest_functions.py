import textwrap
import unittest

from clingo import ast
from clingo.core import Library
from fasp.util.ast import  StatementAST

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
        """
        Utility: parse program, take rules (skip #program directive if present),
        and call rewrite_rules_with_unnested returning list[(stmt, comps)].
        """
        statements = self.parse_program(program)
        # skip the first statement if it is a #program directive
        rules = statements[1:] if len(statements) > 0 and isinstance(statements[0], ast.StatementProgram) else statements
        transformer = UnnestFunctionsTransformer(self.lib, evaluable_functions, set())
        return transformer.rewrite_rules_with_unnested(rules)

    def test_unnest_example(self):
        program = textwrap.dedent("""\
            p(g(h(a,b),c),d(X)) :- q(X).
            q(X) :- g(1,a) = X.
        """).strip()

        evaluable_functions = {
            SymbolSignature("g", 2),
            SymbolSignature("h", 2),
            SymbolSignature("a", 0),
        }

        rewritten = self.apply_transform(program, evaluable_functions)

        lines = ["#program base."]
        for rule_ast, comps in rewritten:
            lines.append(str(rule_ast).strip())
        new_program = "\n".join(lines).strip()

        # build a list of sets of unnested comparisons (one set per rule)
        unnested_sets = [
            set(str(c).replace(" ", "") for c in comps) for (_rule_ast, comps) in rewritten
        ]

        # expected sets:
        expected_set_first_rule = {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"}
        # expected_set_second_rule = {"a=FUN"}  # second rule should not unnest outer g(1,a) but 'a' is an argument and should be unnested

        # Assertions: both expected sets should appear among per-rule unnested sets
        self.assertIn(expected_set_first_rule, unnested_sets)
        # self.assertIn(expected_set_second_rule, unnested_sets)

        # We expect the first rewritten rule to contain p(FUN...,d(X))
        self.assertIn("p(FUN3,d(X)) :- q(X); a=FUN; h(FUN,b)=FUN2; g(FUN2,c)=FUN3.", new_program)



        # expected_program = textwrap.dedent("""\
        #     #program base.
        #     p(FUN1,d(X)) :- q(X), g(FUN2,c)=FUN1, h(FUN3,b)=FUN2, a=FUN3.
        #     q(X) :- g(1,FUN1) = X, a = FUN1.
        # """).strip()
        # expected_rules = [ r1 for r in expected_program.splitlines() if (r1:=r.strip()) ]

        # program = textwrap.dedent("""\
        #     #program base.
        #     g(X,Y) = Z :- true.
        #     h(X,Y) = Z :- true.
        #     a = Z :- true.
        #     f(g(h(a,b),c),d) :- p(X).
        # """).strip()
        # evaluable_functions = get_evaluable_functions(statements)

        # Parse the program to get the AST

        # Suppose evaluable functions: f/2, g/2, h/2, a/0


        # print("Evaluable functions:", evaluable_functions)
        

    def test_non_evaluable_symbolic_and_function(self):
        program = "q(a,b)."
        evaluable_functions = set()
        rewritten = self.apply_transform(program, evaluable_functions)
        # One rule expected
        self.assertEqual(len(rewritten), 1)
        new_rule, comps = rewritten[0]
        self.assertIn("q(a,b)", str(new_rule))
        self.assertEqual(len(comps), 0)
    
    def test_tuple(self):
        program = "r((a,b))."
        evaluable_functions = set()
        rewritten = self.apply_transform(program, evaluable_functions)
        new_rule, comps = rewritten[0]
        self.assertIn("r((a,b))", str(new_rule))
        self.assertEqual(len(comps), 0)

    def test_absolute_unary_binary_operations(self):
        program = "s(abs(-1), -(1), 1+2)."
        evaluable_functions = set()
        rewritten = self.apply_transform(program, evaluable_functions)
        new_rule, comps = rewritten[0]
        text = str(new_rule)
        self.assertIn("s(abs(-1),-1,1+2).", text)
        self.assertEqual(len(comps), 0)
    
    def test_absolute(self):
        program = "p(|f(a)|)."
        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_rule, comps = rewritten[0]
        # head should now contain FUN (unnested inside absolute)
        self.assertEqual("p(|FUN|) :- f(a)=FUN.", str(new_rule))
        comparisons = {str(c).replace(" ", "") for c in comps}
        self.assertIn("f(a)=FUN", comparisons)
    
    def test_absolute_with_evaluable_function(self):
        program = "s(abs(f(a)))."
        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_rule, comps = rewritten[0]
        text = str(new_rule)
        self.assertIn("abs(FUN", text)
        self.assertEqual(len(comps), 1)
        self.assertIn("f(a)=FUN", str(comps[0]).replace(" ", ""))

    def test_non_rule_statement_rewrite(self):
        program = "#const n=10. [default]"
        statements = self.parse_program(program)

        # #const produces a StatementDefinition, not a StatementRule
        stmt = statements[0]

        transformer = UnnestFunctionsTransformer(self.lib, set(), set())
        new_stmt, comps = transformer.rewrite_rule_with_unnested(stmt)

        # Should be unchanged and still not a rule
        self.assertIsInstance(new_stmt, ast.StatementConst)
        self.assertEqual(str(new_stmt).strip(), "#const n=10. [default]")
        self.assertEqual(comps, [])
