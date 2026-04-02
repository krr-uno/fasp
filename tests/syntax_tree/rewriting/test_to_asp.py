import unittest

from funasp.util.ast import ELibrary

from funasp.syntax_tree.types import SymbolSignature
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.syntax_tree.rewritings.to_asp import NormalForm2PredicateTransformer


class TestNormalForm2PredicateTransformer(unittest.TestCase):

    def setUp(self):
        """Set up test fixtures for each test."""
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
        """Test rewrite."""
        self.assertRewrite(
            {"f/1", "g/1"},
            "f(X):=Y :- g(X)=Y.",
            "Ff(X,Y) :- Fg(X,Y).",
        )

    def test_choice_assignment_rewrite(self):
        """Test choice assignment rewrite."""
        self.assertRewrite(
            {"f/1"}, "{ f(X) := Y } :- p.", "{ f_f(X,Y) } :- p.", prefix="f_"
        )

    def test_choice_assignment_invalid(self):
        """Test choice assignment invalid."""
        with self.assertRaises(AssertionError) as cm:
            self.assertRewrite(
                {"f/1"},
                "{ f := Y } :- p.",
                "{ f_f(X,Y) } :- p.",
            )
        self.assertEqual(
            str(cm.exception), "Function f/0 not in evaluable functions {'f/1'}."
        )

    def test_head_simple_assignment(self):
        """Test head simple assignment."""
        self.assertRewrite({"f/1"}, "f(X) := Y :- q.", "f_f(X,Y) :- q.", prefix="f_")

    def test_aggregate_assignment_invalid_head_aggregate_assignment(self):
        """Test aggregate assignment invalid head aggregate assignment."""
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
        """Test aggregate assignment invalid choice some assignment."""
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
        """Test choice assignment with tuple."""
        self.assertRewrite(
            {"a/0"},
            "{ a := (X,Y): p(X,Y) } = 1 :- #count { X,Y: p(X,Y) } >= 1; p.",
            "{ Fa((X,Y)): p(X,Y) } = 1 :- #count { X,Y: p(X,Y) } >= 1; p.",
        )

    def test_head_aggregate_assignment(self):
        """Test head aggregate assignment."""
        self.assertRewrite(
            {"f/1", "g/1", "h/1", "e/1", "king/1"},
            # king/1 is not in evaluable functions. Error?
            "#count { 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
            "#count { 0,ass(king(f(C)),X): Fking(g(C),h(X)): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
        )


    ## TESTS FOR REVIEW ##
    def test_pool_not_evaluable(self):
        """Test pool to_asp transformation: not evaluable function."""
        self.assertRewrite(
            {"a/0", "b/0"},
            "f(1;a,b)=c.",
            "f(1;a,b)=c.",
        )

    # LiteralComparisons get rewritten with prefix by to_asp.
    def test_no_pool_equality_comparison(self):
        """Test to_asp transformation: comparison."""
        self.assertRewrite(
            {"f/1", "b/0"},
            "f(1)=c.",
            "Ff(1,c).",
        )
    
    # HeadSimpleAssignment get rewritten with prefix by to_asp.
    def test_no_pool_assignment(self):
        """Test to_asp transformation: assignment."""
        self.assertRewrite(
            {"f/1", "b/0"},
            "f(1):=c.",
            "Ff(1,c).",
        )

    # Both `f(1)=c.` and `f(1):=c.` get rewritten into `Ff(1,c).` by to_asp. 
    # Is this correct? How do we distinguish between them ?
    # This is how to_asp behaves even before changes to allow it to work on TermFunctions with pools.

    # Same behaviour with pools.
    def test_pool_rewrite_if_f1_is_evaluable(self):
        """Rewrite pooled function equality when f/1 is evaluable."""
        self.assertRewrite(
            {"f/1"},
            "f(1;a,b) = c.",
            "Ff((1,;a,b),c).",
        )

    def test_pool_rewrite_if_f1_is_evaluable_assignment(self):
        """Rewrite pooled assignment when f/1 is evaluable."""
        self.assertRewrite(
            {"f/1"},
            "f(1;a,b) := c.",
            "Ff((1,;a,b),c).",
        )

    def test_pool_rewrite_if_f2_is_evaluable(self):
        """Rewrite pooled function equality when f/2 is evaluable."""
        self.assertRewrite(
            {"f/2"},
            "f(1;a,b) = c.",
            "Ff((1,;a,b),c).",
        )

    def test_pool_rewrite_if_f2_is_evaluable_assignment(self):
        """Rewrite pooled assignment when f/2 is evaluable."""
        self.assertRewrite(
            {"f/2"},
            "f(1;a,b) := c.",
            "Ff((1,;a,b),c).",
        )

    def test_pool_assignment(self):
        """Rewrite pooled assignment when f/2 is evaluable."""
        self.assertRewrite(
            {"f/2"},
            "f(1;a,b) := c.",
            "Ff((1,;a,b),c).",
        )
        