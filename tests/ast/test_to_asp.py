from math import exp
from re import S
import unittest

from fasp.util.ast import ELibrary

from fasp.ast.collectors import SymbolSignature
from fasp.ast.tree_sitter.parser import parse_string
from fasp.ast.to_asp import NormalForm2PredicateTransformer


class TestNormalForm2PredicateTransformer(unittest.TestCase):

    def setUp(self):
        self.lib = ELibrary()

    def assertRewrite(
        self,
        evaluable_functions: set[str],
        program: str,
        expected: str,
    ):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """

        evaluable_functions = [f.split("/") for f in evaluable_functions]
        evaluable_functions = {
            SymbolSignature(name, int(arity)) for name, arity in evaluable_functions
        }
        self.transformer = NormalForm2PredicateTransformer(
            self.lib.library, evaluable_functions
        )
        statements = parse_string(self.lib, program)
        transformed_statements = [self.transformer.rewrite(stmt) for stmt in statements]
        expected = expected.strip().splitlines()
        expected = [l for line in expected if (l := line.strip())]
        self.assertEqual(list(map(str, transformed_statements[1:])), expected)

    def test_rewrite(self):
        self.assertRewrite(
            {"f/1", "g/1"},
            "f(X):=Y :- g(X)=Y.",
            "Ff(X,Y) :- Fg(X,Y).",
        )

