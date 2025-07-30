
from os import PathLike
from pathlib import Path
import unittest

from clingo.core import Library

from fasp.control import Control

EXAMPLES = Path(__file__).parent.parent / "examples"


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

    def test_basic(self):
        self.assert_models(
            [EXAMPLES / "ex01.lp"],
            [
                "b(1) b(2) b(3)",
                "a b(1) b(2) b(3) c(1) c(2) c(3)",
            ],
        )
