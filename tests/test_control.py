from contextlib import redirect_stderr
import io
from os import PathLike
from pathlib import Path
import sys
import textwrap
from typing import Iterable
import unittest

from funasp.control import Control
from funasp.core import Library
from funasp.solve import Model

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
