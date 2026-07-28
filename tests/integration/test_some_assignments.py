"""
Integration tests for ``#some`` assignments.
"""

import unittest

from tests.integration.base import TransformTestCase


class TestSomeAssignments(TransformTestCase):
    def test_some_assignment(self):
        """Test #some assignment."""
        self.assertTransformEqual(
            "color(X) := #some{r;g;b} :- country(X).",
            """
            #count { 0,Fcolor(X,r): Fcolor(X,r); 0,Fcolor(X,g): Fcolor(X,g); 0,Fcolor(X,b): Fcolor(X,b) } = 1 :- country(X).
            :- Fcolor(X0,_); 1 < #count { V: Fcolor(X0,V) }.
            """,
        )
        self.assertTransformEqual(
            "a := #some{X : p(X)} :- b.",
            """
            #count { 0,Fa(X): Fa(X): p(X) } = 1 :- #count { X: p(X) } >= 1; b.
            :- Fa(_); 1 < #count { V: Fa(V) }.
            """,
        )
        self.assertTransformEqual(
            "a := #some{X,Y : p(X,Y)}.",
            """
            #count { 0,Fa((X,Y)): Fa((X,Y)): p(X,Y) } = 1 :- #count { X,Y: p(X,Y) } >= 1.
            :- Fa(_); 1 < #count { V: Fa(V) }.
            """,
        )

    def test_some_assignment_with_pool(self):
        """Test #some assignment with a pooled left guard."""
        self.assertTransformEqual(
            "f(a;b) := #some{r;g}.",
            """
            #count { 0,Ff(a,r): Ff(a,r); 0,Ff(a,g): Ff(a,g) } = 1.
            #count { 0,Ff(b,r): Ff(b,r); 0,Ff(b,g): Ff(b,g) } = 1.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
