import io
import sys
import textwrap
import unittest
from contextlib import redirect_stderr
from os import PathLike
from pathlib import Path
from typing import Iterable

from clingo_funasp.core import MessageType

from funasp.asp2funasp.util.types import FRelation
from funasp.control import Control
from funasp.core import Library
from funasp.solve import Model
from funasp.util.types import SymbolSignature
from tests.examples import EXAMPLES

TEST_EXAMPLES_PATH = Path(__file__).parent / "examples"

ASP2FUNASP_PROGRAM = """
    node(1).
    color(red;blue).
    1 { assign(N,C) : color(C) } 1 :- node(N).
    #show assign/2.
"""


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

    def _solve_program(
        self,
        program: str = ASP2FUNASP_PROGRAM,
        *,
        asp2funasp: bool = True,
        prefix: str = "F",
    ) -> tuple[Control, list[str]]:
        """Parse and solve a string through a configured control."""
        control = Control(
            self.library,
            ["0"],
            prefix=prefix,
            asp2funasp=asp2funasp,
        )
        control.parse_string(textwrap.dedent(program))
        control.ground()
        return control, [str(model) for model in control.solve()]

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

    def test_asp2funasp_disabled_leaves_standard_asp_unchanged(self):
        """Standard ASP retains predicate semantics unless conversion is enabled."""
        control, models = self._solve_program(asp2funasp=False)

        self.assertIsNone(control.conversion_result)
        self.assertCountEqual(models, ["assign(1,red)", "assign(1,blue)"])

    def test_asp2funasp_converts_and_solves_string(self):
        """Detected relations are solved and displayed as function assignments."""
        control, models = self._solve_program()

        result = control.conversion_result
        self.assertIsNotNone(result)
        assert result is not None
        self.assertEqual(
            result.accepted_relations,
            (FRelation("assign", 2, (0,), [(1,)]),),
        )
        self.assertEqual(result.skipped_relations, ())
        self.assertCountEqual(models, ["assign(1)=red", "assign(1)=blue"])

    def test_asp2funasp_honors_custom_prefix(self):
        """Conversion feeds canonical AST through the configured FUNASP prefix."""
        control, models = self._solve_program(prefix="G")

        self.assertIn("Gassign", control.get_rewritten_program())
        self.assertNotIn("Fassign", control.get_rewritten_program())
        self.assertCountEqual(models, ["assign(1)=red", "assign(1)=blue"])

    def test_asp2funasp_converts_and_solves_file(self):
        """File parsing uses the same conversion and solving path as strings."""
        control = Control(self.library, ["0"], asp2funasp=True)
        control.parse_files([str(TEST_EXAMPLES_PATH / "asp2funasp.lp")])
        control.ground()

        self.assertIsNotNone(control.conversion_result)
        self.assertCountEqual(
            [str(model) for model in control.solve()],
            ["assign(1)=red", "assign(1)=blue"],
        )

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
