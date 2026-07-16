"""
Integration tests for negated literals over intensional functions: doubly
negated body and condition literals, negated aggregate element literals, and
negated condition literals lifted into auxiliary rules.
"""

import unittest

from tests.integration.base import TransformTestCase


class TestNegatedLiterals(TransformTestCase):
    def test_not_not(self):
        """Test double negation."""
        self.assertTransformEqual(
            """
            f(X) := 1 :- p(X), not not q(f(X)).
            q(1).
            """,
            """
            Ff(X,1) :- p(X); not not RD1(X).
            RD1(X) :- q(FUN); Ff(X,FUN).
            q(1).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_not_not_with_negated_literal(self):
        """Other body literals are not copied into the auxiliary rule."""
        self.assertTransformEqual(
            "f(X) := 1 :- p(X), not not q(f(X)), not r(X).",
            """
            Ff(X,1) :- p(X); not not RD1(X); #false: r(X).
            RD1(X) :- q(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_not_not_without_global_variables(self):
        """A ground doubly negated literal is lifted to a 0-ary auxiliary."""
        self.assertTransformEqual(
            """
            f(a) := 1.
            b :- not not p(f(a)).
            """,
            """
            Ff(a,1).
            b :- not not RD1.
            RD1 :- p(FUN); Ff(a,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_not_not_without_intensional_functions(self):
        """Function-free doubly negated body literals are left untouched."""
        self.assertTransformEqual(
            "a :- p(X), not not q(X).",
            "a :- p(X); not not q(X).",
        )

    def test_not_not_in_aggregate_condition(self):
        """Doubly negated intensional literals in aggregate conditions are lifted."""
        self.assertTransformEqual(
            """
            f := 1.
            :- 0 < #count{ X : p(X), not not q(f) }.
            """,
            """
            Ff(1).
            :- 0 < #count { X: p(X), not not RD1 }.
            RD1 :- q(FUN); Ff(FUN).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_not_not_in_condition(self):
        """Doubly negated intensional condition literals are lifted."""
        self.assertTransformEqual(
            """
            f(a) := 1.
            a :- b(X) : c(X), not not q(f(X)).
            """,
            """
            Ff(a,1).
            a :- b(X): c(X), not not RD1(X).
            RD1(X) :- q(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_not_not_in_choice_condition(self):
        """Doubly negated intensional literals in choice conditions are lifted."""
        self.assertTransformEqual(
            """
            f(a) := 1.
            { p(X) : q(X), not not r(f(X)) } :- s(X).
            """,
            """
            Ff(a,1).
            #count { 0,p(X): p(X): q(X), not not RD1(X) } :- s(X).
            RD1(X) :- r(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_negated_set_aggregate_literal(self):
        """Test that negated set-aggregate element literals are lifted."""
        self.assertTransformEqual(
            """
            f := 1.
            :- 1 { not p(f) : q }.
            """,
            """
            Ff(1).
            :- 1 <= #count { 1,RD1: q, not RD1 }.
            RD1 :- p(FUN); Ff(FUN).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_intensional_in_negated_head_aggregate_literal(self):
        """Test that negated head-aggregate element literals are lifted."""
        self.assertTransformEqual(
            """
            f := 1.
            1 = #count{ X : not p(f) : q(X) } :- r.
            """,
            """
            Ff(1).
            1 = #count { X: #true: q(X), not RD1 } :- r.
            RD1 :- p(FUN); Ff(FUN).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_intensional_in_double_negated_head_aggregate_literal(self):
        """Test that doubly negated element literals keep their sign when lifted."""
        self.assertTransformEqual(
            """
            f := 1.
            1 = #count{ X : not not p(f) : q(X) } :- r.
            """,
            """
            Ff(1).
            1 = #count { X: #true: q(X), not not RD1 } :- r.
            RD1 :- p(FUN); Ff(FUN).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_intensional_in_negated_element_literal_with_variables(self):
        """Test that lifted element literals carry the literal's variables."""
        self.assertTransformEqual(
            """
            f(a) := 1.
            1 = #count{ X : not p(f(X)) : q(X) } :- r.
            """,
            """
            Ff(a,1).
            1 = #count { X: #true: q(X), not RD1(X) } :- r.
            RD1(X) :- p(FUN); Ff(X,FUN).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_negated_comparison_in_aggregate_condition(self):
        """Negated comparisons needing unnesting are lifted with guards."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            :- 0 < #count{ X : p(X), not f(X)+1 = 3 }.
            """,
            """
            Ff(1,2).
            :- 0 < #count { X: p(X), not RD1(X) }.
            RD1(X) :- p(X); 1*FUN+1=3; Ff(X,FUN); FUN=2.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_double_negated_comparison_in_body(self):
        """A doubly negated body comparison needing unnesting keeps its sign."""
        self.assertTransformEqual(
            "f := 1 :- not not f+0 = 1.",
            """
            Ff(1) :- not not RD1.
            RD1 :- 1*FUN+0=1; Ff(FUN); FUN=1.
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_intensional_in_double_negated_comparison_in_aggregate_condition(self):
        """Doubly negated comparisons needing unnesting keep their sign when lifted."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            :- 0 < #count{ X : p(X), not not f(X)+1 = 3 }.
            """,
            """
            Ff(1,2).
            :- 0 < #count { X: p(X), not not RD1(X) }.
            RD1(X) :- p(X); 1*FUN+1=3; Ff(X,FUN); FUN=2.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_negated_comparison_in_aggregate_condition2(self):
        """Rule-body literals guard globals of a lifted comparison.

        ``Y`` is bound by ``q(Y)`` outside the aggregate, so the auxiliary
        rule copies it as a guard to stay safe.
        """
        self.assertTransformEqual(
            """
            f(1) := 2.
            :- q(Y), 0 < #count{ X : p(X), not f(X)+Y = 3 }.
            """,
            """
            Ff(1,2).
            :- q(Y); 0 < #count { X: p(X), not RD1(X,Y) }.
            RD1(X,Y) :- q(Y); p(X); Ff(X,FUN); FUN+Y=3.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_intensional_in_negated_aggregate_condition(self):
        """Test that negated aggregate condition literals are lifted."""
        self.assertTransformEqual(
            """
            f := 1.
            :- 0 < #count{ X : p(X), not q(f) }.
            """,
            """
            Ff(1).
            :- 0 < #count { X: p(X), not RD1 }.
            RD1 :- q(FUN); Ff(FUN).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_negated_condition_literals_in_head(self):
        """Test that head conditional and choice conditions are lifted."""
        self.assertTransformEqual(
            """
            a(X) : b(X), not c(X) :- d(X).
            { p(X) : q(X), not r(X) } :- s(X).
            """,
            """
            a(X): b(X), not RD1(X) :- d(X).
            RD1(X) :- c(X).
            #count { 0,p(X): p(X): q(X), not RD2(X) } :- s(X).
            RD2(X) :- r(X).
            """,
        )

    def test_intensional_in_negated_condition(self):
        """Test that negated condition literals are lifted into auxiliary rules."""
        self.assertTransformEqual(
            """
            f := 1.
            :- q : not p(f).
            """,
            """
            Ff(1).
            :- q: not RD1.
            RD1 :- p(FUN); Ff(FUN).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            """,
        )

    def test_negated_condition_literals_with_variables(self):
        """Test that lifted condition literals carry the literal's variables."""
        self.assertTransformEqual(
            """
            a :- b(X); c(X,Y) : d(Y), not e(5,f(Y;Y+2)).
            b(2) :- c(X) : d(X), e(Y), not p(g(X,Y)).
            """,
            """
            a :- b(X); c(X,Y): d(Y), not RD1(Y).
            RD1(Y) :- e(5,f(Y)).
            RD1(Y) :- e(5,f(1*Y+2)).
            b(2) :- c(X): d(X), e(Y), not RD2(X,Y).
            RD2(X,Y) :- p(g(X,Y)).
            """,
        )


if __name__ == "__main__":
    unittest.main()
