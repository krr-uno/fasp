import textwrap
import unittest

from clingo import ast
from clingo.core import Library
from fasp.util.ast import StatementAST

from fasp.ast.transformers.unnest_functions import UnnestFunctionsTransformer
from fasp.ast.transformers.unnested_rule_rewrite import UnnestedRuleRewriteTransformer
from fasp.ast.syntax_checking import SymbolSignature


class TestUnnestedRuleRewriteTransformer(unittest.TestCase):

    def setUp(self):
        self.lib = Library()

    def parse_program(self, program: str):
        stmts = []
        ast.parse_string(self.lib, program, stmts.append)
        return stmts

    def apply_transform(self, program: str, evaluable_functions):
        """
        Parse `program`, extract rule statements (skip #program directive if present),
        run UnnestedRuleRewriteTransformer and return its result list of
        (rewritten_statement, unnested_comparisons).
        """
        statements = self.parse_program(program)
        # skip the first statement if it is a #program directive
        rules = (
            statements[1:]
            if len(statements) > 0 and isinstance(statements[0], ast.StatementProgram)
            else statements
        )
        transformer = UnnestedRuleRewriteTransformer(
            self.lib,
            evaluable_functions,
            unnest_transformer_cls=UnnestFunctionsTransformer,
        )
        return transformer.rewrite_rules_with_unnested(rules)

    def construct_new_program(self, rewritten):
        """
        Turn rewrite output into a textual program and also collect per-rule unnested-sets.
        Returns: (program_str, list_of_sets_of_comparisons)
        """
        unnested_sets = []
        lines = ["#program base."]
        for rule_ast, comps in rewritten:
            lines.append(str(rule_ast).strip())
            unnested_sets.append(set(str(c).replace(" ", "") for c in comps))
        return "\n".join(lines).strip(), unnested_sets

    # --- tests (mirrors the tests for the unnest functions transformer) ---

    def test_unnest_example(self):
        program = textwrap.dedent("""\
            p(g(h(a,b),c),d(X)) :- q(X).
            q(X) :- g(1,a) = X.
            g(X,Y) = Z :- true.
        """).strip()

        expected_program = textwrap.dedent("""\
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

        self.assertEqual(len(rewritten), 3)

        # check that unnested comparisons contain expected auxiliaries
        expected_set_first_rule = {"a=FUN", "h(FUN,b)=FUN2", "g(FUN2,c)=FUN3"}
        expected_set_second_rule = {"a=FUN"}

        self.assertIn(expected_set_first_rule, unnested_sets)
        self.assertIn(expected_set_second_rule, unnested_sets)
        self.assertIn(set(), unnested_sets)  # third rule has no unnested

        self.assertEqual(new_program, expected_program)
    
    def test_normalize_lhs_evaluable_rhs_nonevaluable(self):
        program = "p(X) :- f(a)=b."

        expected_program = textwrap.dedent("""\
            #program base.
            p(X) :- f(a)=b.
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, _ = self.construct_new_program(rewritten)
        # f(a) on LHS, b non-evaluable: no rewrite
        print(new_program)
        self.assertEqual(new_program, expected_program)

    def test_normalize_lhs_evaluable_rhs_evaluable(self):
        program = "p(X) :- f(a) = g(b). "

        expeccted_program = textwrap.dedent("""\
            #program base.
            p(X) :- f(a)=FUN; g(b)=FUN.
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, unnested_sets = self.construct_new_program(rewritten)

        self.assertEqual(new_program, expeccted_program)

    
    def test_normalize_lhs_nonevaluable_rhs_evaluable(self):
        program = "p(X) :- a=f(b)."

        expected_program = textwrap.dedent("""\
            #program base.
            p(X) :- f(b)=a.
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, _ = self.construct_new_program(rewritten)

        # RHS evaluable: flip sides
        self.assertEqual(new_program, expected_program)

    def test_normalize_not_equal_operator(self):
        program = "p(X) :- f(a)!=g(b)."

        expected_program = textwrap.dedent("""\
            #program base.
            p(X) :- f(a)!=FUN; g(b)=FUN.
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, _ = self.construct_new_program(rewritten)

        # both evaluable: introduces FUN and !=
        self.assertEqual(new_program, expected_program)
    
    def test_normalize_less_than_operator(self):
        program = "p(X) :- f(a)<g(b)."

        expected_program = textwrap.dedent("""\
            #program base.
            p(X) :- FUN1<FUN2; g(a)=FUN1; g(b)=FUN2.
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, _ = self.construct_new_program(rewritten)
        
        # both evaluable: introduces FUN and <
        self.assertEqual(new_program, expected_program)

    def test_non_rule_statement_rewrite(self):
        program = "#const n=10. [default]"

        # #const produces a StatementDefinition, not a StatementRule
        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, _ = self.construct_new_program(rewritten)

        self.assertIn("#const n=10. [default]", new_program)


    def test_normalize_aggregate(self):
        program = "p(X) :- f(a)<g(b), #sum{ f(X) : p(f(X)) } = 0."

        expected_program = textwrap.dedent("""\
            #program base.
            p(X) :- f(a)<FUN; g(b)=FUN. 
        """).strip()

        evaluable_functions = {SymbolSignature("f", 1), SymbolSignature("g", 1)}
        rewritten = self.apply_transform(program, evaluable_functions)
        new_program, _ = self.construct_new_program(rewritten)
        
        # both evaluable: introduces FUN and <
        self.assertEqual(new_program, expected_program)