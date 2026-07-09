"""
End-to-end tests: parse with the new parser, rewrite with the new pipeline,
then ground and solve, asserting the expected models of the example programs
(the same expectations as ``tests/test_control.py``, without ``Control``).
"""

import unittest

from clingo_funasp import ast
from clingo_funasp.control import Control as ClingoControl

from funasp.ast import RewriteContext, parse_string, rewrite_statements
from funasp.core import Library
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

    def test_negated_condition_auxiliary_predicates_are_hidden(self):
        """Internal RD predicates from negated condition lifting are not printed."""
        program = """
        p(1). q(1). q(2).
        r :- q(X) : not p(X), q(X).
        """

        models = self.get_models(program)

        self.assertEqual(models, ["p(1) q(1) q(2) r"])

    def test_negated_condition_auxiliaries_are_hidden_with_r_prefix(self):
        """RD auxiliaries are not mistaken for function assignments with prefix R."""
        program = """
        p(1). q(1). q(2).
        r :- q(X) : not p(X), q(X).
        """

        models = self.get_models(program, prefix="R")

        self.assertEqual(models, ["p(1) q(1) q(2) r"])

    def test_functions_are_shown_with_auxiliary_like_prefixes(self):
        """Prefixes overlapping the auxiliary prefixes still show function values.

        Regression test: with ``--prefix-fun RD`` (or ``AD``) function atoms
        used to be mistaken for hidden auxiliary predicates and silently
        dropped from the model output.
        """
        program = """
        p(1). q(1). q(2).
        r :- q(X) : not p(X), q(X).
        f(a) := 1.
        """

        for prefix in ("RD", "AD", "RD1"):
            with self.subTest(prefix=prefix):
                models = self.get_models(program, prefix=prefix)

                self.assertEqual(models, ["p(1) q(1) q(2) r\nf(a)=1"])

    def test_double_negated_intensional_function_literal_can_bind(self):
        """Double-negated literals use positive function lookups to bind variables."""
        program = """
        f(a) := 1.
        p(1).
        b :- not not p(f(a)).
        """

        models = self.get_models(program)

        self.assertEqual(models, ["b p(1)\nf(a)=1"])

    def test_double_negated_intensional_function_literal_can_fail(self):
        """Double-negated literals stay false when the function value does not match."""
        program = """
        f(a) := 2.
        p(1).
        b :- not not p(f(a)).
        """

        models = self.get_models(program)

        self.assertEqual(models, ["p(1)\nf(a)=2"])
