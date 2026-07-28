"""
Integration tests for the configured function prefix (``--prefix-fun``):
renaming across assignment shapes, collision validation, and the empty
prefix.
"""

import unittest

from funasp.util.ast import RewritingException
from tests.integration.base import TransformTestCase


class TestPrefixes(TransformTestCase):
    def test_prefix(self):
        """Test the configured prefix is applied across all assignment shapes."""
        self.assertTransformEqual(
            """
            a := 1 :- b.
            f(X) := #sum{Y: p(Y)} :- q(X).
            { c := 1 }.
            p(a).
            #showf a/0.
            """,
            """
            Ga(1) :- b.
            Gf(X,W) :- q(X); W = #sum { Y: p(Y) }.
            #count { 0,Gc(1): Gc(1) }.
            p(FUN) :- Ga(FUN).
            #show Ga/1. [true]
            :- Ga(_); 1 < #count { V: Ga(V) }.
            :- Gc(_); 1 < #count { V: Gc(V) }.
            :- Gf(X0,_); 1 < #count { V: Gf(X0,V) }.
            """,
            prefix="G",
        )

    def test_prefix_some(self):
        """Test the configured prefix with #some assignments and plain statements."""
        self.assertTransformEqual(
            """
            color(X) := #some{r;g;b} :- country(X).
            :- neighbor(C,D), color(C)=color(D).
            country(a).
            :- q.
            #show color/1.
            #count { X: king(C) := X: person(X) } :- country(C).
            1 <= #count{ X: p(X): q(X) } <= 2.
            """,
            """
            #count { 0,Gcolor(X,r): Gcolor(X,r); 0,Gcolor(X,g): Gcolor(X,g); 0,Gcolor(X,b): Gcolor(X,b) } = 1 :- country(X).
            :- neighbor(C,D); Gcolor(C,FUN); Gcolor(D,FUN).
            country(a).
            :- q.
            #show color/1. [true]
            #count { X: Gking(C,X): person(X) } :- country(C).
            1 <= #count { X: p(X): q(X) } <= 2.
            :- Gcolor(X0,_); 1 < #count { V: Gcolor(X0,V) }.
            :- Gking(X0,_); 1 < #count { V: Gking(X0,V) }.
            """,
            prefix="G",
        )

    def test_multi_character_prefix(self):
        """A non-colliding multi-character function prefix is accepted."""
        self.assertTransformEqual(
            """
            g := 1.
            good(a,b).
            """,
            """
            Fung(1).
            good(a,b).
            :- Fung(_); 1 < #count { V: Fung(V) }.
            """,
            prefix="Fun",
        )

    def test_prefix_collision_rejected(self):
        """A function prefix that collides with a user predicate is rejected."""
        with self.assertRaisesRegex(
            RewritingException,
            r"function prefix 'go' collides with predicate\(s\): good/2",
        ):
            self.assertTransformEqual(
                """
                g := 1.
                good(a,b).
                """,
                "",
                prefix="go",
            )

    def test_prefix_collision_with_shown_predicate_rejected(self):
        """Predicates occurring only in ``#show`` statements are checked too."""
        with self.assertRaisesRegex(
            RewritingException,
            r"function prefix 'go' collides with predicate\(s\): good/1",
        ):
            self.assertTransformEqual(
                """
                g := 1.
                #show good/1.
                """,
                "",
                prefix="go",
            )

    def test_prefix_collision_can_be_ignored(self):
        """The collision check can be bypassed explicitly."""
        self.assertTransformEqual(
            """
            g := 1.
            good(a,b).
            """,
            """
            gog(1).
            od(a)=b.
            :- gog(_); 1 < #count { V: gog(V) }.
            """,
            prefix="go",
            ignore_prefix_collisions=True,
        )

    def test_empty_prefix_rejected(self):
        """An empty function prefix is rejected."""
        with self.assertRaisesRegex(
            RewritingException,
            "function prefix must not be empty",
        ):
            self.assertTransformEqual(
                """
                g := 1.
                """,
                "",
                prefix="",
            )

    def test_empty_prefix_rejected_even_when_collisions_ignored(self):
        """An empty function prefix cannot be allowed by ignoring collisions."""
        with self.assertRaisesRegex(
            RewritingException,
            "function prefix must not be empty",
        ):
            self.assertTransformEqual(
                """
                g := 1.
                """,
                "",
                prefix="",
                ignore_prefix_collisions=True,
            )

    def test_empty_program(self):
        """An empty program is rewritten to an empty program without raising."""
        self.assertTransformEqual("", "")

    def test_empty_prefix_rejected_for_empty_program(self):
        """An empty function prefix is rejected even for an empty program."""
        with self.assertRaisesRegex(
            RewritingException,
            "function prefix must not be empty",
        ):
            self.assertTransformEqual("", "", prefix="")

    def test_prefix_disjunction(self):
        """Test that non-assignment heads pass through the renaming pass."""
        self.assertTransformEqual(
            "a | b :- c.",
            "a; b :- c.",
            prefix="G",
        )


if __name__ == "__main__":
    unittest.main()
