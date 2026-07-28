"""
Integration tests for choice rules and head aggregates (braced-choice
assignments, cardinality bounds, and choice conditions).
"""

import unittest

from tests.integration.base import TransformTestCase


class TestChoiceRules(TransformTestCase):
    def test_total_integration(self):
        """Test total integration."""
        self.assertTransformEqual(
            "1 { sk(X, Y)  : skis(Y) } 1 :- sks(X).",
            "1 <= #count { 0,sk(X,Y): sk(X,Y): skis(Y) } <= 1 :- sks(X).",
        )

    def test_king(self):
        """Test king."""
        self.assertTransformEqual(
            """
            country(france).
            country(usa).
            person(felipe).

            {king(C,X) : person(X)}:- country(C).


            :- king(C1,X); king(C2,X); C1!=C2.
            """,
            """
            country(france).
            country(usa).
            person(felipe).
            #count { 0,king(C,X): king(C,X): person(X) } :- country(C).
            :- king(C1,X); king(C2,X); C1!=C2.
            """,
        )

    def test_head_aggregate_assignment2(self):
        """Test head aggregate assignment2."""
        self.assertTransformEqual(
            "{king(spain) := felipe}.",
            """
            #count { 0,Fking(spain,felipe): Fking(spain,felipe) }.
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )

    def test_to_asp_head_aggregate_assignment(self):
        """Test to asp head aggregate assignment."""
        self.assertTransformEqual(
            "#count { 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
            """
            #count { 0,ass(FUN,X): Fking(g(C),h(X)): person(e(X)), Fking(f(C),FUN); ass(FUN2,X): f(X): person(e(X)), Fking(f(C),FUN2) } :- country(C).
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )

        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            """
            #count { 0,Fking(C,X): Fking(C,X): person(X) } :- country(C).
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )

        self.assertTransformEqual(
            """
            a := 1.
            {f(a) := 1}.
            """,
            """
            Fa(1).
            #count { 0,Ff(FUN,1): Ff(FUN,1): Fa(FUN) }.
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

        self.assertTransformEqual(
            """
            a := 1.
            {f(a) := 1} = a.
            """,
            """
            Fa(1).
            #count { 0,Ff(FUN,1): Ff(FUN,1): Fa(FUN) } = FUN2 :- Fa(FUN2).
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_choice_count(self):
        """Test choice count."""
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            #count { Y: p(X): f(X) = Y} :- q(X); r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            #count { Y: p(X): Ff(X,Y) } :- q(X); r.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_choice_count_assignment(self):
        """Test choice count with an embedded assignment."""
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            #count { Y: g(X) := Y: f(X) = Y} :- q(X); r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            #count { Y: Fg(X,Y): Ff(X,Y) } :- q(X); r.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            :- Fg(X0,_); 1 < #count { V: Fg(X0,V) }.
            """,
        )

    def test_choice_condition_unnesting(self):
        """Test intensional functions inside choice assignment conditions."""
        self.assertTransformEqual(
            """
            a := 1.
            { c := 1 : p(a) }.
            """,
            """
            Fa(1).
            #count { 0,Fc(1): Fc(1): p(FUN), Fa(FUN) }.
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Fc(_); 1 < #count { V: Fc(V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
