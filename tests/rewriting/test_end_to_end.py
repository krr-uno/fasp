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

    def test_double_negation_allows_self_supported_values(self):
        """A doubly negated function literal can self-support the assignment.

        As with plain ``a :- not not a.``, both the model without the
        assignment and the self-supported one are stable.
        """
        program = """
        p(1). q(1).
        f(X) := 1 :- p(X), not not q(f(X)).
        """

        models = self.get_models(program)

        self.assertCountEqual(models, ["p(1) q(1)", "p(1) q(1)\nf(1)=1"])

    def test_double_negated_comparison_allows_self_supported_values(self):
        """A doubly negated body comparison needing unnesting keeps its sign.

        ``f+0 = 1`` behaves like the plain equality ``f = 1``: both the model
        without the assignment and the self-supported one are stable.
        """
        program = """
        p(1).
        f := 1 :- p(1), not not f+0 = 1.
        """

        models = self.get_models(program)

        self.assertCountEqual(models, ["p(1)", "p(1)\nf=1"])

    def test_double_negated_comparison_in_aggregate_condition(self):
        """Doubly negated comparisons in aggregate conditions keep their sign."""
        program = """
        p(1).
        f := 1 :- 1 <= #count{ X : p(X), not not f+0 = 1 }.
        """

        models = self.get_models(program)

        self.assertCountEqual(models, ["p(1)", "p(1)\nf=1"])

    def test_weak_body_aggregate_guard_keeps_lifted_condition(self):
        """A weak body's aggregate guard carries its lifted condition.

        ``f(2)`` is undefined, so a stale copy of the original aggregate —
        whose condition demands ``Ff`` positively after unnesting — would
        suppress the weak constraint; the rewritten guard yields cost 1.
        """
        program = """
        f(1) := 2.
        p(2).
        :~ X = #count{ Y : p(Y), not q(f(Y)) }, not not f(X)+1 = 3. [1@0,X]
        """
        context = RewriteContext(self.library, "F")
        statements = parse_string(self.library, program)
        rewritten = rewrite_statements(context, statements)
        control = ClingoControl(self.library.library, ["0"])
        prog = ast.Program(self.library.library)
        for wrapper in rewritten:
            for statement in wrapper.rewritten:
                prog.add(statement)
        control.join(prog)
        control.ground([("base", ())])
        costs = []
        with control.start_solve(yield_=True) as handle:
            for model in handle:
                costs.append(list(model.cost))

        self.assertEqual(costs, [[1]])

    def test_double_negation_with_negated_literal(self):
        """A negated literal alongside a lifted double negation still blocks."""
        program = """
        p(1). q(1). r(1).
        f(X) := 1 :- p(X), not not q(f(X)), not r(X).
        """

        models = self.get_models(program)

        self.assertEqual(models, ["p(1) q(1) r(1)"])

    def test_double_negation_in_aggregate_condition(self):
        """Doubly negated function literals in aggregate conditions test values."""
        program = """
        f := {value}. p(1). q(1).
        b :- 0 < #count{{ X : p(X), not not q(f) }}.
        """

        matching = self.get_models(program.format(value=1))
        mismatching = self.get_models(program.format(value=2))

        self.assertEqual(matching, ["b p(1) q(1)\nf=1"])
        self.assertEqual(mismatching, ["p(1) q(1)\nf=2"])

    def test_double_negation_in_condition(self):
        """Doubly negated function literals in conditions test values.

        With ``f(a)=1`` the condition holds and demands ``b(a)`` (absent, so
        ``a`` is not derived); with ``f(a)=2`` the condition fails and the
        conditional literal is vacuously true.
        """
        program = """
        f(a) := {value}. c(a). q(1).
        a :- b(X) : c(X), not not q(f(X)).
        """

        holding = self.get_models(program.format(value=1))
        vacuous = self.get_models(program.format(value=2))

        self.assertEqual(holding, ["c(a) q(1)\nf(a)=1"])
        self.assertEqual(vacuous, ["a c(a) q(1)\nf(a)=2"])

    def test_negated_comparison_in_aggregate_condition(self):
        """Negated comparisons over functions hold when the value fails or is undefined.

        Regression test: the lookup used to be emitted positively, so an
        undefined function silently falsified the negated comparison.
        """
        program = """
        {assignment} p(1). b :- 0 < #count{{ X : p(X), not f+1 = 3 }}.
        """

        undefined = self.get_models(program.format(assignment="f := 1 :- c."))
        mismatching = self.get_models(program.format(assignment="f := 1."))
        matching = self.get_models(program.format(assignment="f := 2."))

        self.assertEqual(undefined, ["b p(1)"])
        self.assertEqual(mismatching, ["b p(1)\nf=1"])
        self.assertEqual(matching, ["p(1)\nf=2"])

    def test_negated_comparison_with_global_variable(self):
        """A lifted comparison with an outer-bound variable stays safe and correct."""
        program = """
        {assignment} q(1). p(1).
        :- q(Y), 0 < #count{{ X : p(X), not f(X)+Y = 3 }}.
        """

        matching = self.get_models(program.format(assignment="f(1) := 2."))
        mismatching = self.get_models(program.format(assignment="f(1) := 1."))
        undefined = self.get_models(program.format(assignment="f(1) := 2 :- c."))

        self.assertEqual(matching, ["p(1) q(1)\nf(1)=2"])
        self.assertEqual(mismatching, [])
        self.assertEqual(undefined, [])

    def test_negated_element_literal(self):
        """Negated intensional element literals count exactly the failing values."""
        program = """
        f := 1. q(1). r.
        1 = #count{ X : not p(f) : q(X) } :- r.
        """

        self.assertEqual(self.get_models(program), ["q(1) r\nf=1"])
        self.assertEqual(self.get_models("p(1). " + program), [])

    def test_double_negated_element_literal(self):
        """Doubly negated intensional element literals count the holding values."""
        program = """
        f := 1. q(1). r.
        1 = #count{ X : not not p(f) : q(X) } :- r.
        """

        self.assertEqual(self.get_models("p(1). " + program), ["p(1) q(1) r\nf=1"])
        self.assertEqual(self.get_models(program), [])

    def test_negated_element_literal_with_undefined_function(self):
        """An undefined function value satisfies a negated element literal."""
        program = """
        f := 1 :- c. q(1). r.
        1 = #count{ X : not p(f) : q(X) } :- r.
        """

        self.assertEqual(self.get_models(program), ["q(1) r"])

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
