from os import PathLike
from pathlib import Path
import unittest
import io, contextlib


from fasp.app import main

from .examples import EXAMPLES, Example

TEST_EXAMPLES_PATH = Path(__file__).parent / "examples"


class TestControl(unittest.TestCase):

    def execute_app(self, files: PathLike) -> tuple[str, str]:
        args = [str(file) for file in files] + ["0"]
        output_io = io.StringIO()
        err_io = io.StringIO()
        with contextlib.redirect_stdout(output_io):
            with contextlib.redirect_stderr(err_io):
                main(args)
        return output_io.getvalue(), err_io.getvalue()

    def assert_models(self, files: PathLike, expected_models):
        models = []
        is_first_line = True
        output, _ = self.execute_app(files)
        result = None
        for line in output.strip().splitlines():
            line = line.strip()
            if line.startswith("Answer"):
                is_first_line = True
                continue
            if line in {"SATISFIABLE", "UNSATISFIABLE"}:
                result = line
                continue
            if is_first_line:
                is_first_line = False
                models.append(line)
            else:
                models[-1] += "\n" + line
        self.assertIsNotNone(result, "Expected SATISFIABLE or UNSATISFIABLE in output")
        self.assertEqual(result, "SATISFIABLE" if expected_models else "UNSATISFIABLE")
        self.assertCountEqual(models, expected_models)

    def test_app(self):
        examples  = EXAMPLES + [
            Example([TEST_EXAMPLES_PATH / "ex02_fun_fact.lp"], ["f=1"]),
        ]
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
            [example_file],
            ["--mode=rewrite"]
        )

        # Rewrite with custom prefix (G)
        rewrite_custom, _ = self.execute_app_with_args(
            [example_file],
            ["--mode=", "rewrite", "--prefix-fun=G"]
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
