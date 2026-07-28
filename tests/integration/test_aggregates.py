"""
Integration tests for aggregate assignments and intensional-function
occurrences in body aggregates (guards, tuples, elements, and conditions).
"""

import unittest

from tests.integration.base import TransformTestCase


class TestAggregates(TransformTestCase):
    def test_aggregate_assignment(self):
        """Test aggregate assignment."""
        self.assertTransformEqual(
            "f(X) := #sum{Y: p(Y,Z)} :- b(X,Z).",
            """
            Ff(X,W) :- b(X,Z); W = #sum { Y: p(Y,Z) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_aggregate_assignment_with_pool(self):
        """A pooled aggregate target expands to one assignment per entry."""
        self.assertTransformEqual(
            """
            c.
            f(a;b) := #sum{1 : c}.
            """,
            """
            c.
            Ff(a,W) :- W = #sum { 1: c }.
            Ff(b,W) :- W = #sum { 1: c }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_aggregate_assignment_with_mixed_arity_pool(self):
        """A pooled aggregate target expands entries of different arities."""
        self.assertTransformEqual(
            """
            c.
            f(a;b,d) := #sum{1 : c}.
            """,
            """
            c.
            Ff(a,W) :- W = #sum { 1: c }.
            Ff(b,d,W) :- W = #sum { 1: c }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            :- Ff(X0,X1,_); 1 < #count { V: Ff(X0,X1,V) }.
            """,
        )

    def test_company(self):
        """Test company."""
        self.assertTransformEqual(
            "controller(C3) := C1 :- company(C1), company(C3), #sum{controlsStk(C1,C2,C3), C2} > 50.",
            """
            Fcontroller(C3,C1) :- company(C1); company(C3); #sum { FUN,C2: FcontrolsStk(C1,C2,C3,FUN) } > 50.
            :- Fcontroller(X0,_); 1 < #count { V: Fcontroller(X0,V) }.
            :- FcontrolsStk(X0,X1,X2,_); 1 < #count { V: FcontrolsStk(X0,X1,X2,V) }.
            """,
            intensional_functions={"controlsStk/3"},
        )

    def test_aggregates2(self):
        """Test body aggregates with functional conditions."""
        self.assertTransformEqual(
            """
            :- #sum{X : f = X; Y : f = Y} > 0.
            :- #sum{X : f = X; Y : p(Z), f(Z) = Y} > 0.
            """,
            """
            :- #sum { X: Ff(X); Y: Ff(Y) } > 0.
            :- #sum { X: Ff(X); Y: p(Z), Ff(Z,Y) } > 0.
            :- Ff(_); 1 < #count { V: Ff(V) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
            intensional_functions={"f/0", "f/1"},
        )

    def test_aggregates3(self):
        """Test body aggregates with negated functional conditions."""
        self.assertTransformEqual(
            """
            :- #sum{X : f = X; Y : p(Y), not f = Y} > 0.
            :- #sum{X : f = X; Y : p(Z,Y), not f(Z) = Y} > 0.
            """,
            """
            :- #sum { X: Ff(X); Y: p(Y), not Ff(Y) } > 0.
            :- #sum { X: Ff(X); Y: p(Z,Y), not Ff(Z,Y) } > 0.
            :- Ff(_); 1 < #count { V: Ff(V) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
            intensional_functions={"f/0", "f/1"},
        )

    def test_aggregates4(self):
        """Test body aggregates with anonymous functional conditions."""
        self.assertTransformEqual(
            """
            :- #sum{X : f = X; Y : p(Y), not f(Y) = _} > 0.
            """,
            """
            :- #sum { X: Ff(X); Y: p(Y), not Ff(Y,*) } > 0.
            :- Ff(_); 1 < #count { V: Ff(V) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
            intensional_functions={"f/0", "f/1"},
        )

    def test_aggregates5(self):
        """Test body aggregates with anonymous functional conditions."""
        self.assertTransformEqual(
            """
            :- #sum{X : f = X; Y : Y=1, not f = _} > 0.
            """,
            """
            :- #sum { X: Ff(X); 1: not Ff(*) } > 0.
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
            intensional_functions={"f/0"},
        )

    def test_body_set_aggregate_guard_unnesting(self):
        """Intensional functions in body set-aggregate guards are unnested."""
        self.assertTransformEqual(
            """
            f(a) := 1.
            p(1).
            r :- f(a) { p(X) }.
            """,
            """
            Ff(a,1).
            p(1).
            r :- Ff(a,FUN); FUN <= #count { 0,p(X): p(X) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_head_aggregate_element_unnesting(self):
        """Test intensional functions inside head aggregate elements."""
        self.assertTransformEqual(
            """
            a := 1.
            #count{ X: king(a) := X: p(X,a) }.
            """,
            """
            Fa(1).
            #count { X: Fking(FUN2,X): p(X,FUN), Fa(FUN), Fa(FUN2) }.
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
