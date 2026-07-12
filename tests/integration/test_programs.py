"""
Integration tests for full example programs combining several constructs.
"""

import unittest

from tests.integration.base import TransformTestCase


class TestPrograms(TransformTestCase):
    def test_family_full(self):
        """Test family full."""
        self.assertTransformEqual(
            """
            father(cain):=adam.
            father(abel):=adam.
            mother(cain):=eve.
            mother(abel):=eve.

            % person(father(X)).
            % person(mother(X)).
            person(Y) :- father(_)=Y.
            person(Y) :- mother(_)=Y.
            person(X) :- father(X)=_.
            person(X) :- mother(X)=_.
            % male(father(X)).
            % female(mother(X)).
            male(Y) :- father(_)=Y.
            female(Y) :- mother(_)=Y.

            orphan(X) :- person(X), not father(X)=_, not mother(X)=_.

            n_orphan := #count{X : orphan(X)}.
            """,
            """
            Ffather(cain,adam).
            Ffather(abel,adam).
            Fmother(cain,eve).
            Fmother(abel,eve).
            person(Y) :- Ffather(_,Y).
            person(Y) :- Fmother(_,Y).
            person(X) :- Ffather(X,_).
            person(X) :- Fmother(X,_).
            male(Y) :- Ffather(_,Y).
            female(Y) :- Fmother(_,Y).
            orphan(X) :- person(X); #false: Ffather(X,*); #false: Fmother(X,*).
            Fn_orphan(W) :- W = #count { X: orphan(X) }.
            :- Ffather(X0,_); 1 < #count { V: Ffather(X0,V) }.
            :- Fmother(X0,_); 1 < #count { V: Fmother(X0,V) }.
            :- Fn_orphan(_); 1 < #count { V: Fn_orphan(V) }.
            """,
        )

    def test_hamiltonian(self):
        """Test body aggregates with anonymous functional conditions."""
        self.assertTransformEqual(
            """
            next(X) := #some{Y: edge(X,Y)} :- vertex(X).
            start := #min{X: vertex(X)}.
            visited(next(start)).
            visited(next(X)) :- visited(X).
            :- vertex(X), not visited(X).
            """,
            """
            #count { 0,Fnext(X,Y): Fnext(X,Y): edge(X,Y) } = 1 :- vertex(X); #count { Y: edge(X,Y) } >= 1.
            Fstart(W) :- W = #min { X: vertex(X) }.
            visited(FUN2) :- Fstart(FUN); Fnext(FUN,FUN2).
            visited(FUN) :- visited(X); Fnext(X,FUN).
            :- vertex(X); #false: visited(X).
            :- Fnext(X0,_); 1 < #count { V: Fnext(X0,V) }.
            :- Fstart(_); 1 < #count { V: Fstart(V) }.
            """,
            intensional_functions={"start/0", "next/1"},
        )


if __name__ == "__main__":
    unittest.main()
