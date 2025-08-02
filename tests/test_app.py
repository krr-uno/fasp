import enum
from math import exp
from os import PathLike
from pathlib import Path
import textwrap
import unittest
import io, contextlib

from clingo.core import Library

from fasp.app import main

from .examples import EXAMPLES


class TestControl(unittest.TestCase):

    def assert_models(self, files: PathLike, expected_models):
        args = [str(file) for file in files] + ["0"]
        output_io = io.StringIO()
        with contextlib.redirect_stdout(output_io):
            main(args)
        models = []
        is_first_line = True
        for line in output_io.getvalue().strip().splitlines():
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
        for i, example in enumerate(EXAMPLES):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                self.assert_models(example.files, example.models)
