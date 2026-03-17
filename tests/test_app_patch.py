
from os import PathLike
from pathlib import Path
import sys
import unittest
import io, contextlib
from unittest.mock import patch, MagicMock


from funasp.app import FaspApp, main
from funasp.control import Control

from .examples import EXAMPLES

TEST_EXAMPLES_PATH = Path(__file__).parent / "examples"

APP_NAME = "funasp"

FaspAppMock = MagicMock(spec=FaspApp)
# FaspAppMock.prefix = "F"
FaspAppMock._control = MagicMock(spec=Control)
FaspAppMock._control.prefix = "F"
FaspAppMock._control.print_model = lambda model, default_printer: Control.print_model(FaspAppMock._control, model, default_printer)

def control_main_patch(self):
    self.ground()
    for i, model in enumerate(self.solve()):
        sys.stdout.write(f"Answer {i + 1}:\n")
        FaspApp.print_model(FaspAppMock, model.clingo_model, lambda: None)
    if self._result is not None:
        if self._result.satisfiable:
            sys.stdout.write("SATISFIABLE\n")
        elif self._result.unsatisfiable:
            sys.stdout.write("UNSATISFIABLE\n")

class TestControl(unittest.TestCase):

    def execute_app(self, files: PathLike) -> tuple[str, str]:
        args = [str(file) for file in files] + ["0"]
        args_main = args + ["--outf=3"]  # For testing rewrite mode
        output_io = io.StringIO()
        err_io = io.StringIO()
        with contextlib.redirect_stdout(output_io):
                with contextlib.redirect_stderr(err_io):
                    main(args_main)
        with patch("funasp.control.Control.main", new=control_main_patch):
            with contextlib.redirect_stdout(output_io):
                with contextlib.redirect_stderr(err_io):
                    main(args_main)
        return output_io.getvalue(), err_io.getvalue()

    def assert_models(self, files: PathLike, expected_models, *, allow_errors: bool=False):
        models = []
        line_number = 0
        output, error = self.execute_app(files)
        if not allow_errors:
            self.assertEqual(error.strip(),"")
        result = None
        for line in output.strip().splitlines():
            line = line.strip()
            if line.startswith("Answer"):
                line_number = 1
                continue
            if line in {"SATISFIABLE", "UNSATISFIABLE"}:
                result = line
                line_number = 0
                continue
            if line_number == 1:
                line_number += 1
                models.append(line)
            elif line_number > 1:
                models[-1] += "\n" + line
        self.assertIsNotNone(result, "Expected SATISFIABLE or UNSATISFIABLE in output")
        self.assertEqual(result, "SATISFIABLE" if expected_models else "UNSATISFIABLE")
        # print(models)
        self.assertCountEqual(models, expected_models)

    def test_app(self):
        examples = EXAMPLES
        # examples += [
        #     Example([TEST_EXAMPLES_PATH / "ex02_fun_fact.lp"], ["f=1"]),
        # ]
        for i, example in enumerate(examples):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                self.assert_models(example.files, example.models)

    # def test_syntactic_error(self):
    #     output, err = self.execute_app([TEST_EXAMPLES_PATH / "ex01_syntactic_error.lp"])
    #     self.assertEqual(output.strip(), "")
    #     self.assertEqual(
    #         list(map(lambda x: x.strip(), err.splitlines()[0].strip().split(":")[1:])),
    #         [
    #             "1",
    #             "1-7",
    #             "error",
    #             "syntax error, unexpected comparison a>5 in the head. Assignments are of the form 'FUNCTION = TERM'.",
    #         ],
    #     )
    #     self.assertEqual(
    #         err.splitlines()[1].strip(), "*** ERROR: (fasp): parsing failed"
    #     )

    # def test_prefix_option(self):
    #     example_file = TEST_EXAMPLES_PATH / "ex02_fun_fact.lp"

    #     # Run with default prefix (F)
    #     output_default, _ = self.execute_app([example_file])

    #     # Run with custom prefix
    #     args = [str(example_file), "--prefix=G", "0"]
    #     output_io = io.StringIO()
    #     err_io = io.StringIO()
    #     with contextlib.redirect_stdout(output_io):
    #         with contextlib.redirect_stderr(err_io):
    #             main(args)
    #     output_custom = output_io.getvalue()

    #     # Ensure outputs are equal even when prefix changes
    #     self.assertEqual(output_default, output_custom)

    def execute_app_with_args(self, files, extra_args):
        args = [str(file) for file in files] + extra_args + ["0"]
        output_io = io.StringIO()
        err_io = io.StringIO()
        with contextlib.redirect_stdout(output_io):
            with contextlib.redirect_stderr(err_io):
                main(args)
        return output_io.getvalue(), err_io.getvalue()

    def test_prefix_and_print_rewrite(self):
        example_file = TEST_EXAMPLES_PATH / "ex02_fun_fact.lp"

        # Rewrite with default prefix (F)
        rewrite_default, _ = self.execute_app_with_args(
            [example_file], ["--mode=rewrite"]
        )

        # Rewrite with custom prefix (G)
        rewrite_custom, _ = self.execute_app_with_args(
            [example_file], ["--mode=", "rewrite", "--prefix-fun=G"]
        )

        # Both rewrites should be non-empty
        self.assertTrue(rewrite_default.strip())
        self.assertTrue(rewrite_custom.strip())

        # Default should contain F-prefixed predicates
        self.assertIn("F", rewrite_default)

        # Custom should contain G-prefixed predicates
        self.assertIn("G", rewrite_custom)

        # Rewrites must differ
        self.assertNotEqual(rewrite_default, rewrite_custom)

    def test_app_syntax_error(self):
        example_file = TEST_EXAMPLES_PATH / "syntax_error.lp"

        # NOTE: No Error raised? app.py line:67-68
        out, err = self.execute_app(
            [example_file]
        )
        self.assertIn("syntax error", err)
        self.assertIn("*** ERROR: (fasp): parsing failed", err)

    def test_app_unsafe(self):
        example_file = TEST_EXAMPLES_PATH / "unsafe.lp"

        # NOTE: No Error raised? app.py line:67-68
        out, err = self.execute_app(
            [example_file]
        )
        self.assertIn("rewriting failed", err)
        self.assertIn("UNKNOWN", out)
        self.assertIn("*** ERROR: (fasp): rewriting failed", err)

    def test_app_undefined_function(self):
        example_file = TEST_EXAMPLES_PATH / "undefined_function.lp"

        # NOTE: No Error raised? app.py line:67-68
        out, err = self.execute_app(
            [example_file]
        )
        self.assertIn("undefined intensional function a/1", err)
        self.assert_models(
            [example_file],
            [""],
            allow_errors=True,
        )
