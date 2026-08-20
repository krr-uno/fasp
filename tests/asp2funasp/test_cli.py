"""Tests for the standalone ``asp2funasp`` source converter."""

import contextlib
import io
import tempfile
import textwrap
import unittest
from pathlib import Path
from unittest.mock import patch

from funasp.asp2funasp.cli import main
from funasp.ast import parse_string
from funasp.core import Library

EXPECTED_PROGRAM = """\
#program base.
node(1).
color(red;blue).
1 <= { assign(N) := C: color(C) } <= 1 :- node(N).
#showf assign/1.
"""


class Asp2FunaspCliTest(unittest.TestCase):
    def _run(
        self,
        *arguments: str,
        stdin: str = "",
    ) -> tuple[int, str, str]:
        stdout = io.StringIO()
        stderr = io.StringIO()
        with (
            patch("sys.stdin", io.StringIO(stdin)),
            contextlib.redirect_stdout(stdout),
            contextlib.redirect_stderr(stderr),
        ):
            result = main(arguments)
        return result, stdout.getvalue(), stderr.getvalue()

    def test_converts_file_to_parseable_funasp_on_stdout(self) -> None:
        input_path = Path(__file__).parents[1] / "examples" / "asp2funasp.lp"

        result, output, error = self._run(str(input_path))

        self.assertEqual(result, 0)
        self.assertEqual(output, EXPECTED_PROGRAM)
        self.assertEqual(error, "")
        with Library() as library:
            self.assertTrue(parse_string(library, output))

    def test_reads_standard_asp_from_stdin(self) -> None:
        source = """
            node(1).
            color(red;blue).
            1 { assign(N,C) : color(C) } 1 :- node(N).
            #show assign/2.
        """

        result, output, error = self._run("-", stdin=textwrap.dedent(source))

        self.assertEqual((result, output, error), (0, EXPECTED_PROGRAM, ""))

    def test_writes_converted_program_to_output_file(self) -> None:
        input_path = Path(__file__).parents[1] / "examples" / "asp2funasp.lp"
        with tempfile.TemporaryDirectory() as directory:
            output_path = Path(directory) / "converted.lp"

            result, output, error = self._run(
                str(input_path),
                f"--out={output_path}",
            )

            self.assertEqual((result, output, error), (0, "", ""))
            self.assertEqual(output_path.read_text(encoding="utf-8"), EXPECTED_PROGRAM)

    def test_reports_parser_errors(self) -> None:
        result, output, error = self._run("-", stdin="p(")

        self.assertEqual(result, 65)
        self.assertEqual(output, "")
        self.assertIn("syntax error", error)

    def test_reports_output_errors(self) -> None:
        input_path = Path(__file__).parents[1] / "examples" / "asp2funasp.lp"
        with tempfile.TemporaryDirectory() as directory:
            result, output, error = self._run(
                str(input_path),
                f"--out={directory}",
            )

        self.assertEqual(result, 1)
        self.assertEqual(output, "")
        self.assertIn("asp2funasp: error:", error)


if __name__ == "__main__":
    unittest.main()
