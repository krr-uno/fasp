"""Structured choice keys must preserve the contexts making each choice."""

import textwrap
import unittest

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.pattern_finders.structured_choice_inputs import (
    constructor_variables,
    structured_choice_inputs,
)
from tests.asp2funasp.util import ConversionTestCase, collect_statements


class StructuredChoiceConversionTest(ConversionTestCase):

    def test_edge_assignment_and_lookups(self) -> None:
        self.assertConversionEqual(
            """
            { edge_value(edge(X,Y),N) : num(N), N>0 } = 1 :- edge(X,Y).
            seen(X,Y,N) :- edge_value(edge(X,Y),N).
            missing(X,Y) :- edge(X,Y), not edge_value(edge(X,Y),1).
            #show edge_value/2.
            """,
            """
            #program base.
            { edge_value(edge(X,Y)) := N: num(N), N>0 } = 1 :- edge(X,Y).
            seen(X,Y,N) :- edge_value(edge(X,Y))=N.
            missing(X,Y) :- edge(X,Y); not edge_value(edge(X,Y))=1.
            #showf edge_value/1.
            """,
        )

    def test_models_and_empty_domain(self) -> None:
        for numbers in ("num(0).", "num(0;1;2)."):
            source = f"""
                edge(a,b).
                edge(b,c).
                {numbers}
                {{ edge_value(edge(X,Y),N) : num(N), N>0 }} = 1 :- edge(X,Y).
                seen(X,Y,N) :- edge_value(edge(X,Y),N).
                missing(X,Y) :- edge(X,Y), not edge_value(edge(X,Y),1).
                #show seen/3.
                #show missing/2.
            """
            with self.subTest(numbers=numbers):
                expected = self._models(source)
                self.assertEqual(len(expected), 0 if numbers == "num(0)." else 4)
                self.assertEqual(self._models(source, True), expected)
                self.assertEqual(self._models(self._convert_source(source)), expected)

    def test_nested_tuple_and_multiple_outputs(self) -> None:
        for key in ("wrap(edge(X,Y),tag)", "(X,Y)"):
            with self.subTest(key=key):
                source = f"""
                    {{ pick({key},A,B) : option(A,B) }} = 1 :- edge(X,Y).
                    seen(X,Y,A,B) :- pick({key},A,B).
                """
                result, _ = self.assertConversionEqual(
                    source,
                    f"""
                    #program base.
                    {{ pick({key}) := (A,B): option(A,B) }} = 1 :- edge(X,Y).
                    seen(X,Y,A,B) :- pick({key})=(A,B).
                    """,
                )
                facts = "edge(a,b). option(left,horizontal). "
                projection = " #show seen/4."
                self.assertEqual(
                    self._models(facts + source + projection),
                    self._models(facts + result + projection),
                )

    def test_rejects_unfixed_or_lost_context(self) -> None:
        rules = (
            "{ pick(edge(X,Z),N) : option(Z,N) } = 1 :- node(X).",
            "{ pick(edge(X,Y),N) : option(Z,N) } = 1 :- triple(X,Y,Z).",
            "{ pick(edge(X+Y),N) : num(N) } = 1 :- edge(X,Y).",
            "{ pick(edge(X,Y),N) : num(N) } = 1 :- node(X), Y=X+1.",
            "{ pick(edge(X,Y),N) : num(N) } = 1 :- edge(X,Y), bound(N).",
        )
        for rule in rules:
            with self.subTest(rule=rule):
                self.assertNotIn(":=", self._convert_source(rule))

    def test_lost_context_counterexample_preserves_two_values(self) -> None:
        source = """
            triple(a,b,c).
            triple(a,b,d).
            option(c,1).
            option(d,2).
            { pick(edge(X,Y),N) : option(Z,N) } = 1 :- triple(X,Y,Z).
            #show pick/2.
        """
        expected = {"pick(edge(a,b),1) pick(edge(a,b),2)"}
        self.assertEqual(self._models(source), expected)
        self.assertEqual(self._models(source, True), expected)


class StructuredChoiceInputsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    def _apply(self, program: str) -> ast.StatementRule:
        """Parse a single rule using the existing pattern-finder test helper."""
        return collect_statements(self.lib, textwrap.dedent(program).strip())[0]

    def test_plain_variable_is_an_input(self) -> None:
        ordinary = self._apply("p(X) :- q(X).")
        self.assertEqual(
            structured_choice_inputs(ordinary, ordinary.head.literal, []), [0]
        )

    def test_boolean_head_has_no_inputs(self) -> None:
        ordinary = self._apply("p(X) :- q(X).")
        boolean = self._apply(":- q(X).").head.literal
        self.assertEqual(structured_choice_inputs(ordinary, boolean, []), [])

    def test_constructor_rejects_unsupported_terms(self) -> None:
        for source in ("p(_).", "p(f(_)).", "p(X+1).", "p(f(a;b))."):
            with self.subTest(source=source):
                term = self._apply(source).head.literal.atom.pool[0].arguments[0]
                self.assertIsNone(constructor_variables(term))

    def test_negation_does_not_fix_constructor_variables(self) -> None:
        negative = self._apply("p(f(X),N) :- not q(X).")
        self.assertEqual(
            structured_choice_inputs(negative, negative.head.literal, [1]), []
        )

    def test_arithmetic_does_not_fix_constructor_variables(self) -> None:
        unsupported = self._apply("p(f(X),N) :- q(X+1).")
        self.assertEqual(
            structured_choice_inputs(unsupported, unsupported.head.literal, [1]), []
        )


if __name__ == "__main__":
    unittest.main()
