"""
End-to-end tests: parse with the new parser, rewrite with the new pipeline,
then ground and solve, asserting the expected models of the example programs
(the same expectations as ``tests/test_control.py``, without ``Control``).
"""

import unittest

from clingo_funasp import ast
from clingo_funasp.control import Control as ClingoControl

from funasp.ast import parse_string
from funasp.core import Library
from funasp.ast.rewriting._context import RewriteContext
from funasp.ast.rewriting import rewrite_statements
from funasp.solve import Model
from tests.examples import EXAMPLES


class TestEndToEnd(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.library = Library()

    def get_models(self, program: str, prefix: str = "F") -> list[str]:
        """Parse, rewrite, ground, and solve a program; return model strings."""
        context = RewriteContext(self.library, prefix)
        statements = parse_string(self.library, program)
        rewritten = rewrite_statements(context, statements)
        control = ClingoControl(self.library.library, ["0"])
        prog = ast.Program(self.library.library)
        for wrapper in rewritten:
            for statement in wrapper.rewritten:
                prog.add(statement)
        control.join(prog)
        control.ground([("base", ())])
        models = []
        with control.start_solve(yield_=True) as handle:
            for model in handle:
                models.append(str(Model(model, prefix)))
        return models

    def test_examples(self):
        """All example programs yield their expected models."""
        for i, example in enumerate(EXAMPLES):
            file_names = [f.name for f in example.files]
            with self.subTest(f"{i}: {file_names}"):
                program = "\n".join(f.read_text() for f in example.files)
                models = self.get_models(program)
                self.assertCountEqual(models, example.models)

    def test_coloring_prefix(self):
        """The coloring example solves with a custom prefix too."""
        coloring = EXAMPLES[7]
        assert coloring.files[0].name == "coloring.lp"
        program = coloring.files[0].read_text()
        models = self.get_models(program, prefix="G")
        self.assertCountEqual(models, coloring.models)
