"""
Integration tests for disjunctive heads.
"""

import unittest

from funasp.util.ast import RewritingException
from tests.integration.base import TransformTestCase


class TestDisjunctions(TransformTestCase):
    def test_disjunction(self):
        """Function-free disjunctive heads pass through unchanged."""
        self.assertTransformEqual(
            "a | b :- c.",
            "a; b :- c.",
        )

    def test_intensional_function_in_disjunctive_head_rejected(self):
        """Intensional terms in disjunctive heads produce a semantic error."""
        with self.assertRaisesRegex(
            RewritingException,
            r"error: intensional functions are not allowed in disjunctive heads: 'f\(a\)'",
        ):
            self.assertTransformEqual(
                """
                f(a) := 1.
                p(f(a)) | q.
                """,
                None,
            )


if __name__ == "__main__":
    unittest.main()
