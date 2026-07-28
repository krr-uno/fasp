"""
Integration tests for optimize statements and weak constraints with
intensional-function occurrences (tuples, bodies, and negated literals).
"""

import unittest

from tests.integration.base import TransformTestCase


class TestOptimize(TransformTestCase):
    def test_optimize(self):
        """Test optimize statements and weak constraints."""
        self.assertTransformEqual(
            "#minimize{ X : f = X }.",
            """
            :~ Ff(X). [X]
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
            intensional_functions={"f/0"},
        )
        self.assertTransformEqual(
            ":~ p(X); f = X. [X@1]",
            """
            :~ p(X); Ff(X). [X@1]
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
            intensional_functions={"f/0"},
        )

    def test_optimize_unnesting(self):
        """Test intensional functions inside optimize elements and weak constraints."""
        self.assertTransformEqual(
            """
            a := 1.
            #minimize{ f(a),X : p(X,a) }.
            """,
            """
            Fa(1).
            :~ p(X,FUN2); Fa(FUN); Fa(FUN2). [f(FUN),X]
            :- Fa(_); 1 < #count { V: Fa(V) }.
            """,
        )
        self.assertTransformEqual(
            """
            a := 1.
            :~ q(a). [a@1]
            """,
            """
            Fa(1).
            :~ q(FUN2); Fa(FUN); Fa(FUN2). [FUN@1]
            :- Fa(_); 1 < #count { V: Fa(V) }.
            """,
        )

    def test_intensional_in_negated_weak_constraint_literal(self):
        """Test that negated weak-constraint body literals are lifted."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            :~ p(X), not q(f(X)). [1@0,X]
            """,
            """
            Ff(1,2).
            :~ p(X); not RD1(X). [1@0,X]
            RD1(X) :- q(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_double_negated_weak_constraint_literal(self):
        """Test that doubly negated weak-constraint body literals keep their sign."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            :~ p(X), not not q(f(X)). [1@0,X]
            """,
            """
            Ff(1,2).
            :~ p(X); not not RD1(X). [1@0,X]
            RD1(X) :- q(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_negated_weak_constraint_comparison(self):
        """Negated comparisons needing unnesting are lifted with guards."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            :~ p(X), not f(X)+1 = 3. [1@0,X]
            """,
            """
            Ff(1,2).
            :~ p(X); not RD1(X). [1@0,X]
            RD1(X) :- p(X); 1*FUN+1=3; Ff(X,FUN); FUN=2.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_negated_optimize_condition(self):
        """Test that negated optimize element condition literals are lifted."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            #minimize { 1,X : p(X), not q(f(X)) }.
            """,
            """
            Ff(1,2).
            :~ p(X); not RD1(X). [1,X]
            RD1(X) :- q(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
