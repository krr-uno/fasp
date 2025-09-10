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
    
    def transform(self, stmts: StatementAST, evaluable_functions):
        used_vars = set()
        transformer = UnnestFunctionsTransformer(self.lib, evaluable_functions, used_vars)
        
        new_stmts = transformer.transform_rule(stmts)
        return new_stmts, transformer.unnested_functions

    def test_unnest_example(self):

        program = textwrap.dedent("""\
            p(g(h(a,b),c),d(X)) :- q(X).
        """).strip()

        evaluable_functions = {
            SymbolSignature("g", 2),
            SymbolSignature("h", 2),
            SymbolSignature("a", 0),
        }

        statements = self.parse_program(program)
        rules = statements[1:]  # skip #program directive
        for rule in rules: # pass each rule separately
            new_stmts, unnested = self.transform(rule, evaluable_functions)
            new_head = str(new_stmts.head).replace(" ", "")
            self.assertIn("p(FUN3,d(X))", new_head)

        # Check the LiteralComparisons
        comparisons = set(str(c).replace(" ", "") for c in unnested)
        self.assertEqual(comparisons, {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"})

        


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
        

        # Check the resulting rule head contains FUN3 instead of nested functions
        # for stmt in new_stmts:
        #     print(str(stmt))
        # print(unnested)
        # for unn in unnested:
        #     print(str(unn))



    def test_non_evaluable_symbolic_and_function(self):
        program = "q(a,b)."
        stmts = self.parse_program(program)
        # no evaluable functions
        evaluable_functions = set()
        statements = self.parse_program(program)
        rules = statements[1:]  # or just statements if no #program
        for rule in rules:
            new_stmts, unnested = self.transform(rule, evaluable_functions)


            # program stays the same
            self.assertIn("q(a,b)", str(new_stmts))
            self.assertEqual(len(unnested), 0)

    def test_tuple(self):
        program = "r((a,b))."
        stmts = self.parse_program(program)
        evaluable_functions = set()
        statements = self.parse_program(program)
        rules = statements[1:]  # or just statements if no #program
        for rule in rules:
            new_stmts, unnested = self.transform(rule, evaluable_functions)

            # Check tuple structure is unmodified
            self.assertIn("r((a,b))", str(new_stmts))
            self.assertEqual(len(unnested), 0)

    def test_absolute_unary_binary_operations(self):
        program = "s(abs(-1), -(1), 1+2)."
        stmts = self.parse_program(program)
        evaluable_functions = set()
        new_stmts, unnested = self.transform(program, evaluable_functions)

        text = str(new_stmts)
        self.assertIn("abs", text)
        self.assertIn("-", text)
        self.assertIn("+", text)
        self.assertEqual(len(unnested), 0)

    def test_absolute(self):
        program = "p(|f(a)|)."
        evaluable_functions = {SymbolSignature("f", 1)}
        
        statements = self.parse_program(program)
        rules = statements[1:]  # skip #program directive
        
        for rule in rules:
            new_stmts, unnested = self.transform(rule, evaluable_functions)
            new_head = str(new_stmts.head).replace(" ", "")
            self.assertIn("FUN", new_head)
            # also check that unnested got the comparison
            comparisons = {str(c).replace(" ", "") for c in unnested}
            self.assertIn("f(a)=FUN", comparisons)

    
    def test_absolute_with_evaluable_function(self):
        program = "s(abs(f(a)))."
        evaluable_functions = {SymbolSignature("f", 1)}

        statements = self.parse_program(program)
        rules = statements[1:]  # or just statements if no #program
        for rule in rules:
            new_stmts, unnested = self.transform(rule, evaluable_functions)

            text = str(new_stmts)
            self.assertIn("abs(FUN", text)

            self.assertEqual(len(unnested), 1)
            self.assertIn("f(a)=FUN", str(unnested[0]).replace(" ", ""))