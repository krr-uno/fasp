from contextlib import redirect_stderr
import io
from os import PathLike
from pathlib import Path
import sys
import textwrap
from typing import Iterable
import unittest

from clingo_funasp.core import MessageType

from funasp.control import Control
from funasp.core import Library
from funasp.solve import Model
from funasp.util.types import SymbolSignature

from tests.examples import EXAMPLES

TEST_EXAMPLES_PATH = Path(__file__).parent / "examples"


class TestControl(unittest.TestCase):

    def setUp(self):
        """Set up test fixtures for each test."""
        self.library = Library()

    def get_models(self, files: PathLike) -> Iterable[Model]:
        """Return models."""
        control = Control(self.library, ["0"])

        control.parse_files(list(map(str, files)))
        control.ground()
        for model in control.solve():
            yield model

    def assert_models(self, files: PathLike, expected_models):
        """Assert models."""
        models = [str(model) for model in self.get_models(files)]
        self.assertCountEqual(models, expected_models)

    def test_app(self):
        """Test app."""
        for i, example in enumerate(EXAMPLES):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                self.assert_models(example.files, example.models)

    def test_get_rewritten_program_before_parse(self):
        """Requesting the rewritten program before parsing raises a ValueError."""
        control = Control(self.library, ["0"])
        with self.assertRaises(ValueError):
            control.get_rewritten_program()

    def test_program_const_interval_solves(self):
        """Program ``#const`` definitions feed full function rewriting."""
        control = Control(self.library, ["0"])
        control.parse_string(
            """
            #const n = 3.
            f(X):=X :- p(X).
            p(1..n).
            q(V) :- f(n)=V.
            #show q/1.
            #showf f/1.
            """
        )
        control.ground()

        models = [str(model) for model in control.solve()]

        self.assertEqual(models, ["q(3)\nf(1)=1 f(2)=2 f(3)=3"])

    def test_command_line_const_interval_solves(self):
        """Command-line constants feed full function rewriting."""
        control = Control(self.library, ["0", "-c", "n=3"])
        control.parse_string(
            """
            f(X):=X :- p(X).
            p(1..n).
            q(V) :- f(n)=V.
            #show q/1.
            #showf f/1.
            """
        )
        control.ground()

        models = [str(model) for model in control.solve()]

        self.assertEqual(models, ["q(3)\nf(1)=1 f(2)=2 f(3)=3"])

    def test_command_line_const_overrides_program_const(self):
        """Command-line constants keep override semantics through functions."""
        control = Control(self.library, ["0", "-c", "n=3"])
        control.parse_string(
            """
            #const n = 2.
            f(X):=X :- p(X).
            p(1..n).
            q(V) :- f(n)=V.
            #show q/1.
            #showf f/1.
            """
        )
        control.ground()

        models = [str(model) for model in control.solve()]

        self.assertEqual(models, ["q(3)\nf(1)=1 f(2)=2 f(3)=3"])

    def test_undefined_function_log_uses_configured_prefix(self):
        """Undefined function predicates are reported as intensional functions."""
        self.library.prefix_function = "G"
        self.library.function_predicates = {SymbolSignature("Ga", 1)}

        message = self.library.normalize_log_message(
            MessageType.OperationUndefined,
            "<string>:1:1-1: info: undefined predicate Ga/1",
        )

        self.assertEqual(
            message,
            "<string>:1:1-1: info: undefined intensional function a/1",
        )

    def test_functional_undefined_log_uses_configured_prefix(self):
        """Functional undefined-predicate messages honor custom prefixes."""
        self.library.prefix_function = "G"
        self.library.function_predicates = {SymbolSignature("Ga", 1)}

        message = self.library.normalize_log_message(
            MessageType.OperationUndefined,
            "<functional>:0:0-0: info: undefined predicate Ga/1",
        )

        self.assertIsNone(message)

    def test_undefined_user_predicate_log_is_not_mangled(self):
        """Messages about non-function predicates are reported verbatim.

        Regression test: a user predicate whose name starts with the function
        prefix (here ``good`` with prefix ``go``) used to be reported as an
        undefined intensional function ``od/1``.
        """
        self.library.prefix_function = "go"
        self.library.function_predicates = {SymbolSignature("gog", 1)}

        message = self.library.normalize_log_message(
            MessageType.OperationUndefined,
            "<string>:1:1-1: info: undefined predicate good/1",
        )

        self.assertEqual(message, "<string>:1:1-1: info: undefined predicate good/1")

    def _logged_messages(
        self, program: str, prefix: str = "F", ignore_prefix_collisions: bool = False
    ) -> list[str]:
        """Parse, ground, and solve a program; return the logged messages."""
        logged: list[str] = []
        library = Library(logger=lambda _, message: logged.append(message))
        control = Control(
            library,
            ["0"],
            prefix=prefix,
            ignore_prefix_collisions=ignore_prefix_collisions,
        )
        control.parse_string(program)
        control.ground()
        list(control.solve())
        return logged

    def test_shown_undefined_function_message(self):
        """A ``#showf`` of an unassigned function reports an intensional function."""
        logged = self._logged_messages("#showf a/0.")

        self.assertEqual(
            logged, ["<string>:1:1-12: info: undefined intensional function a/1"]
        )

    def test_shown_undefined_user_predicate_message(self):
        """A shown user predicate starting with the prefix is reported verbatim."""
        logged = self._logged_messages(
            "g := 1. #show good/1.", prefix="go", ignore_prefix_collisions=True
        )

        self.assertEqual(logged, ["<string>:1:9-22: info: undefined predicate good/1"])

    def test_undefined_operation_fun(self):
        """Test unsafe.

        Note: with the clingo_funasp parser the location points at the
        undefined operation in the source.
        """
        library = Library(logger=lambda _, message: print(message, file=sys.stderr))
        control = Control(library, ["0"])
        out = io.StringIO()
        with redirect_stderr(out):
            control.parse_string("""
                f := a+1.
                """)
            captured_output = out.getvalue().strip()
            self.assertEqual(
                textwrap.dedent(captured_output),
                textwrap.dedent("""\
                <string>:2:22-25: info: operation undefined in:
                  f := a+1.
                note: the following operations are undefined:
                  a+1"""),
            )
        out = io.StringIO()
        with redirect_stderr(out):
            control.ground()
            captured_output = out.getvalue().strip()
            self.assertEqual(captured_output, "")
