import unittest

from fasp.util.ast import ELibrary

from fasp.syntax_tree.collectors import SymbolSignature
from fasp.syntax_tree.parsing.parser import parse_string
from fasp.syntax_tree.to_asp import NormalForm2PredicateTransformer


class TestNormalForm2PredicateTransformer(unittest.TestCase):

    def setUp(self):
        self.lib = ELibrary()

    def assertRewrite(
        self,
        evaluable_functions: set[str],
        program: str,
        expected: str,
        prefix: str = "F",
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
            self.lib.library, evaluable_functions, prefix=prefix
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

    def test_choice_assignment_rewrite(self):
        self.assertRewrite(
            {"f/1"}, "{ f(X) := Y } :- p.", "{ f_f(X,Y) } :- p.", prefix="f_"
        )

    def test_choice_assignment_invalid(self):
        with self.assertRaises(AssertionError) as cm:
            self.assertRewrite({"f/1"}, "{ f := Y } :- p.", "{ f_f(X,Y) } :- p.")
        self.assertEqual(
            str(cm.exception), "Function f/0 not in evaluable functions {'f/1'}."
        )

    def test_head_simple_assignment(self):
        self.assertRewrite({"f/1"}, "f(X) := Y :- q.", "f_f(X,Y) :- q.", prefix="f_")

    def test_aggregate_assignment_invalid_head_aggregate_assignment(self):
        with self.assertRaises(AssertionError) as cm:
            self.assertRewrite(
                {"score/1"},
                "score(X) := #sum{f(Y): f(p(Y)), q(X) } :- p.",
                "#sum{ f_f(X,Y) } :- q.",
            )
        self.assertEqual(
            str(cm.exception),
            "HeadAggregateAssignment seen during Head AST rewrite during Normalization. This should not happen.",
        )

    def test_aggregate_assignment_invalid_choice_some_assignment(self):
        with self.assertRaises(AssertionError) as cm:
            self.assertRewrite(
                {"score/1"},
                "a := #some{X: p(X)} :- p.",
                "{a := X: p(X)} = 1 :- #count{X: p(X)} >=  1, p.",
            )
        self.assertEqual(
            str(cm.exception),
            "ChoiceSomeAssignment seen during Head AST rewrite during Normalization. This should not happen.",
        )

    def test_choice_assignment_with_tuple(self):
        self.assertRewrite(
            {"a/0"},
            "{ a := (X,Y): p(X,Y) } = 1 :- #count { X,Y: p(X,Y) } >= 1; p.",
            "{ Fa((X,Y)): p(X,Y) } = 1 :- #count { X,Y: p(X,Y) } >= 1; p.",
        )
