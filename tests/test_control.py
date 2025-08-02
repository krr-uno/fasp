from os import PathLike
from pathlib import Path
import unittest

from clingo.core import Library

from fasp.control import Control

from .examples import EXAMPLES


class TestControl(unittest.TestCase):

    def setUp(self):
        self.library = Library()

    def assert_models(self, files: PathLike, expected_models):
        control = Control(self.library, ["0"])

        control.parse_files(list(map(str, files)))
        control.ground()
        models = []
        for model in control.solve():
            models.append(str(model))
        self.assertCountEqual(models, expected_models)

    def test_app(self):
        for i, example in enumerate(EXAMPLES):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                self.assert_models(example.files, example.models)