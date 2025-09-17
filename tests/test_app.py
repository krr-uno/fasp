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
        for line in output.strip().splitlines():
            line = line.strip()
            if line.startswith("Answer"):
                is_first_line = True
                continue
            if is_first_line:
                is_first_line = False
                models.append(line)
            else:
                models[-1] += "\n" + line
        self.assertCountEqual(models, expected_models)

    def test_app(self):
        EXAMPLES.append(Example([TEST_EXAMPLES_PATH / "ex02_fun_fact.lp"], ["f=1"]))
        for i, example in enumerate(EXAMPLES):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                self.assert_models(example.files, example.models)

    def test_syntactic_error(self):
        output, err = self.execute_app([TEST_EXAMPLES_PATH / "ex01_syntactic_error.lp"])
        self.assertEqual(output.strip(), "")
        self.assertEqual(
            list(map(lambda x: x.strip(), err.splitlines()[0].strip().split(":")[1:])),
            [
                "1",
                "1-7",
                "error",
                "syntax error, unexpected comparison a>5 in the head. Assignments are of the form 'FUNCTION = TERM'.",
            ],
        )
        self.assertEqual(
            err.splitlines()[1].strip(), "*** ERROR: (fasp): parsing failed"
        )
    # Adapt to new clingo version change in app.fasp_main 
    def test_unexpected_exception_paths(self):
        import fasp.app as app

        def fake_clingo_main(library, options, fasp_app):
            raise RuntimeError("simulated failure")

        original_clingo_main = app.clingo_main
        app.clingo_main = fake_clingo_main
        try:
            # Case 1: raise_errors = False
            # handled with stderr + exit code
            err_io = io.StringIO()
            with contextlib.redirect_stderr(err_io):
                exit_code = app.main([])

            self.assertEqual(exit_code, 1)
            self.assertIn("*** ERROR: (fasp): simulated failure", err_io.getvalue())

            # Case 2: raise_errors = True
            # exception re-raised
            with self.assertRaises(RuntimeError):
                app.fasp_main(app.Library(), options=[], raise_errors=True)

        finally:
            app.clingo_main = original_clingo_main


