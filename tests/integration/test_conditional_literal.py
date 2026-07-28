"""
Integration tests for conditional literals with intensional-function
occurrences in their main literal or condition.
"""

import unittest

from tests.integration.base import TransformTestCase


class TestConditionalLiteral(TransformTestCase):
    def test_conditional_literal(self):
        """Test conditional literals with functional conditions."""
        self.assertTransformEqual(
            """
            :- p(X); q(X) : f = X.
            :- p(X); q(X) : not f = X.
            :- p(X,Y); q(X) : g(X) = Y.
            :- p(X,Y); q(X) : not g(X) = Y.
            """,
            """
            :- p(X); q(X): Ff(X).
            :- p(X); q(X): not Ff(X).
            :- p(X,Y); q(X): Fg(X,Y).
            :- p(X,Y); q(X): not Fg(X,Y).
            :- Ff(_); 1 < #count { V: Ff(V) }.
            :- Fg(X0,_); 1 < #count { V: Fg(X0,V) }.
            """,
            intensional_functions={"f/0", "g/1"},
        )

    def test_conditional_literal_main(self):
        """Test an intensional function in the main literal of a conditional literal.

        The generated equality must stay inside the condition: its variables
        may be local to the conditional literal.
        """
        self.assertTransformEqual(
            """
            a := 1.
            :- p(X); q(a) : r(X).
            """,
            """
            Fa(1).
            :- p(X); q(FUN): r(X), Fa(FUN).
            :- Fa(_); 1 < #count { V: Fa(V) }.
            """,
        )


if __name__ == "__main__":
    unittest.main()
