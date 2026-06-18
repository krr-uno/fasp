import unittest

from contextlib import redirect_stderr
import io
import sys
import textwrap
import unittest


from funasp.control import Control
from funasp.core import Library

class TestGrounding(unittest.TestCase):

    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library(logger=lambda t, m: print(m, file=sys.stderr))
        self.control = Control(self.lib)
        self.maxDiff = None  # Show full diff on assertion failure

    def assertGroundingEqual(
        self,
        program: str,
        message: str
    ):
        program = textwrap.dedent(program).strip()
        message = textwrap.dedent(message).strip()
        self.control.parse_string(program)
        with io.StringIO() as buf, redirect_stderr(buf):
            self.control.ground()
            stderr = buf.getvalue().strip()
            self.assertEqual(stderr, message)

    def test_clingo_rule(self):
        self.assertGroundingEqual(
            """
            p(X+1) :- q(X).
            q(a).
            """,
            """
            <string>:1:3-4: info: number expected (got a)
            """
        )

    def test_assignment_rule(self):
        self.assertGroundingEqual(
            """
            f := X+1 :- q(X).
            q(a).
            """,
            """
            <string>:1:6-7: info: number expected (got a)
            """
        )