"""
Integration tests for assignment rules and intensional-function occurrences
in rule bodies (unnesting, equalities, and comparisons).
"""

import unittest

from tests.integration.base import TransformTestCase


class TestAssignments(TransformTestCase):
    def test_to_asp(self):
        """Test to asp."""
        self.assertTransformEqual(
            "f(1) := Y :- g(Y).",
            """
            Ff(1,Y) :- g(Y).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_fact(self):
        """Test fact."""
        self.assertTransformEqual(
            """
            c := 1.
            p(c).
            """,
            """
            Fc(1).
            p(FUN) :- Fc(FUN).
            :- Fc(_); 1 < #count { V: Fc(V) }.
            """,
        )

    def test_pool(self):
        """Test pool."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            p(f(a;b,c;d,e,f)).
            """,
            """
            Ff(1,2).
            p(FUN) :- Ff(a,FUN).
            p(FUN) :- f(b,c)=FUN.
            p(FUN) :- f(d,e,f)=FUN.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_assignment_rule(self):
        """Test assignment rule."""
        self.assertTransformEqual(
            """
            c := 1.
            a := b :- p(c).
            p(c).
            """,
            """
            Fc(1).
            Fa(b) :- p(FUN); Fc(FUN).
            p(FUN) :- Fc(FUN).
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Fc(_); 1 < #count { V: Fc(V) }.
            """,
        )

    def test_no_change(self):
        """Test no change."""
        self.assertTransformEqual(
            "f(X) :- g(X).",
            "f(X) :- g(X).",
        )

    def test_fibo(self):
        """Test fibo."""
        self.assertTransformEqual(
            "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
            """
            Ffibo(X,Y) :- number(X); Ffibo(1*X+(-1),FUN); Ffibo(1*X+(-2),FUN2); X>1; Y=FUN+FUN2.
            :- Ffibo(X0,_); 1 < #count { V: Ffibo(X0,V) }.
            """,
        )

    def test_ground_nested_function(self):
        """Test ground nested intensional functions (symbolic terms)."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            p(g(f(1))).
            """,
            """
            Ff(1,2).
            p(g(FUN)) :- Ff(1,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )
        self.assertTransformEqual(
            """
            f(1) := 2.
            p(g(h(f(1)))).
            """,
            """
            Ff(1,2).
            p(g(h(FUN))) :- Ff(1,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_flipped_equality(self):
        """Test an equality with the intensional function on the right side."""
        self.assertTransformEqual(
            """
            f := 1.
            p(X) :- q(X); X = f.
            """,
            """
            Ff(1).
            p(X) :- q(X); Ff(X).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_comparison_guard(self):
        """Test an intensional function in a non-equality comparison guard."""
        self.assertTransformEqual(
            """
            f := 1.
            p(X) :- q(X); X < f.
            """,
            """
            Ff(1).
            p(X) :- q(X); Ff(FUN); X<FUN.
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_non_intensional_equality(self):
        """Test equalities over non-intensional functions are left untouched.

        The ground comparison is constant-folded away by clingo's rewriting,
        which drops the trivially unsatisfiable rule.
        """
        self.assertTransformEqual(
            """
            f := 1.
            p :- q(g(1;2)); g(1) = 2.
            """,
            """
            Ff(1).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
