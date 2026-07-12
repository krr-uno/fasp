"""
Integration tests for error reporting during rewriting (unsafe variables and
undefined operations).
"""

import io
import textwrap
import unittest
from contextlib import redirect_stderr

from tests.integration.base import TransformTestCase


class TestErrors(TransformTestCase):
    def test_unsafe(self):
        """Test unsafe."""
        out = io.StringIO()
        with redirect_stderr(out):
            with self.assertRaisesRegex(
                RuntimeError,
                r"\('rewriting failed', \[\(<clingo_funasp\.ast\.StatementRule object at 0x[0-9A-Fa-f]+>, RuntimeError\('rewriting failed'\)\)\]\)",
            ):
                self.assertTransformEqual(
                    """
                    p(X) :- q(Y).
                    """,
                    "",
                )
            captured_output = out.getvalue().strip()
            self.assertEqual(
                captured_output,
                textwrap.dedent("""\
                <string>:1:1-14: error: unsafe variables in:
                  p(X) :- q(Y).
                note: the following variables are unsafe:
                  X"""),
            )

    def test_unsafe_fun(self):
        """Test unsafe assignment."""
        out = io.StringIO()
        with redirect_stderr(out):
            with self.assertRaisesRegex(
                RuntimeError,
                r"\('rewriting failed', \[\(<clingo_funasp\.ast\.StatementRule object at 0x[0-9A-Fa-f]+>, RuntimeError\('rewriting failed'\)\)\]\)",
            ):
                self.assertTransformEqual(
                    """
                    f := X :- q(Y).
                    """,
                    "",
                )
            captured_output = out.getvalue().strip()
            self.assertEqual(
                captured_output,
                textwrap.dedent("""\
                <string>:1:1-16: error: unsafe variables in:
                  f := X :- q(Y).
                note: the following variables are unsafe:
                  X"""),
            )

    def test_undefined_operation_fun(self):
        """Test undefined operation in an assignment."""
        out = io.StringIO()
        with redirect_stderr(out):
            self.assertTransformEqual(
                """
                f := a + 1.
                """,
                """
                :- Ff(_); 1 < #count { V: Ff(V) }.
                """,
            )
            captured_output = out.getvalue().strip()
            self.assertEqual(
                captured_output,
                textwrap.dedent("""\
                <string>:1:6-11: info: operation undefined in:
                  f := a+1.
                note: the following operations are undefined:
                  a+1"""),
            )


if __name__ == "__main__":
    unittest.main()
