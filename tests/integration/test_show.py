"""
Integration tests for ``#show`` statements and the ``#showf`` directive.
"""

import unittest

from tests.integration.base import TransformTestCase


class TestShow(TransformTestCase):
    def test_show(self):
        """Test show statements with functional conditions."""
        self.assertTransformEqual(
            """
            #show f(X) : f = X.
            #show g(X,Y) : g(X) = Y.
            """,
            """
            #show f(X): Ff(X).
            #show g(X,Y): Fg(X,Y).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            :- Fg(X0,_); 1 < #count { V: Fg(X0,V) }.
            """,
            intensional_functions={"f/0", "g/1"},
        )

    def test_show_negation(self):
        """Test show statements with negated functional conditions."""
        self.assertTransformEqual(
            """
            #show f(X) : dom(X), not f = X.
            #show g(X,Y) : dom(X,Y), not g(X) = Y.
            """,
            """
            #show f(X): dom(X); not Ff(X).
            #show g(X,Y): dom(X,Y); not Fg(X,Y).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            :- Fg(X0,_); 1 < #count { V: Fg(X0,V) }.
            """,
            intensional_functions={"f/0", "g/1"},
        )

    def test_showf_directive(self):
        """Test #showf directive."""
        self.assertTransformEqual(
            """
            #showf color/1.
            color(X) := #some{r;g} :- c(X).
            """,
            """
            #show Fcolor/2. [true]
            #count { 0,Fcolor(X,r): Fcolor(X,r); 0,Fcolor(X,g): Fcolor(X,g) } = 1 :- c(X).
            :- Fcolor(X0,_); 1 < #count { V: Fcolor(X0,V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
