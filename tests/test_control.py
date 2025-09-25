from os import PathLike
from pathlib import Path
from typing import Iterable
import unittest


from fasp.control import Control
from fasp.util.ast import ELibrary
from fasp.solve import Model

from .examples import EXAMPLES

TEST_EXAMPLES_PATH = Path(__file__).parent / "examples"


class TestControl(unittest.TestCase):

    def setUp(self):
        self.library = ELibrary()

    def get_models(self, files: PathLike) -> Iterable[Model]:
        control = Control(self.library, ["0"])

        control.parse_files(list(map(str, files)))
        control.ground()
        for model in control.solve():
            yield model

    def assert_models(self, files: PathLike, expected_models):
        models = [str(model) for model in self.get_models(files)]
        self.assertCountEqual(models, expected_models)

    def test_app(self):
        for i, example in enumerate(EXAMPLES):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                self.assert_models(example.files, example.models)

    # def test_syntactic_error(self):
    #     with self.assertRaises(ParsingException) as e:
    #         next(self.get_models([TEST_EXAMPLES_PATH / "ex01_syntactic_error.lp"]))
