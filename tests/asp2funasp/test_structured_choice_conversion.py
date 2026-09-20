"""Structured choice keys must preserve the contexts making each choice."""

import unittest

from clingo_funasp import ast

from funasp.asp2funasp import convert_statements
from funasp.asp2funasp.pattern_finders.structured_choice_inputs import (
    constructor_variables,
    structured_choice_inputs,
)
from funasp.ast import ast_to_str, parse_string
from funasp.control import Control
from funasp.core import Library


class StructuredChoiceConversionTest(unittest.TestCase):
    def convert(self, source):
        with Library() as lib:
            result = convert_statements(
                lib.library, [s.original for s in parse_string(lib, source)]
            )
            return "\n".join(ast_to_str(s) for s in result.converted_statements)

    def models(self, source, convert=False):
        with Library(logger=lambda *_: None) as lib:
            control = Control(lib, ["0"], asp2funasp=convert, prefix="G")
            control.parse_string(source)
            control.ground()
            return {str(m) for m in control.solve()}

    def test_edge_assignment_and_lookups(self):
        result = self.convert(
            "{ edge_value(edge(X,Y),N) : num(N), N>0 } = 1 :- edge(X,Y). "
            "seen(X,Y,N) :- edge_value(edge(X,Y),N). "
            "missing(X,Y) :- edge(X,Y), not edge_value(edge(X,Y),1). "
            "#show edge_value/2."
        )
        self.assertIn("{ edge_value(edge(X,Y)) := N: num(N), N>0 } = 1", result)
        self.assertIn("seen(X,Y,N) :- edge_value(edge(X,Y))=N.", result)
        self.assertIn("not edge_value(edge(X,Y))=1", result)
        self.assertIn("#showf edge_value/1.", result)

    def test_models_and_empty_domain(self):
        for numbers in ("num(0).", "num(0;1;2)."):
            source = (
                "edge(a,b). edge(b,c). "
                + numbers
                + " { edge_value(edge(X,Y),N) : num(N), N>0 } = 1 :- edge(X,Y). "
                "seen(X,Y,N) :- edge_value(edge(X,Y),N). "
                "missing(X,Y) :- edge(X,Y), not edge_value(edge(X,Y),1). "
                "#show seen/3. #show missing/2."
            )
            with self.subTest(numbers=numbers):
                expected = self.models(source)
                self.assertEqual(len(expected), 0 if numbers == "num(0)." else 4)
                self.assertEqual(self.models(source, True), expected)
                self.assertEqual(self.models(self.convert(source)), expected)

    def test_nested_tuple_and_multiple_outputs(self):
        for key in ("wrap(edge(X,Y),tag)", "(X,Y)"):
            source = (
                f"{{ pick({key},A,B) : option(A,B) }} = 1 :- edge(X,Y). "
                f"seen(X,Y,A,B) :- pick({key},A,B)."
            )
            result = self.convert(source)
            self.assertIn(f"pick({key}) := (A,B)", result)
            self.assertIn(f"pick({key})=(A,B)", result)
            facts = "edge(a,b). option(left,horizontal). "
            projection = " #show seen/4."
            self.assertEqual(
                self.models(facts + source + projection),
                self.models(facts + result + projection),
            )

    def test_rejects_unfixed_or_lost_context(self):
        rules = (
            "{ pick(edge(X,Z),N) : option(Z,N) } = 1 :- node(X).",
            "{ pick(edge(X,Y),N) : option(Z,N) } = 1 :- triple(X,Y,Z).",
            "{ pick(edge(X+Y),N) : num(N) } = 1 :- edge(X,Y).",
            "{ pick(edge(X,Y),N) : num(N) } = 1 :- node(X), Y=X+1.",
            "{ pick(edge(X,Y),N) : num(N) } = 1 :- edge(X,Y), bound(N).",
        )
        for rule in rules:
            with self.subTest(rule=rule):
                self.assertNotIn(":=", self.convert(rule))

    def test_lost_context_counterexample_preserves_two_values(self):
        source = (
            "triple(a,b,c). triple(a,b,d). option(c,1). option(d,2). "
            "{ pick(edge(X,Y),N) : option(Z,N) } = 1 :- triple(X,Y,Z). "
            "#show pick/2."
        )
        expected = {"pick(edge(a,b),1) pick(edge(a,b),2)"}
        self.assertEqual(self.models(source), expected)
        self.assertEqual(self.models(source, True), expected)

    def test_helper_rejects_unsupported_terms(self):
        with Library() as lib:

            def rule(source):
                return next(
                    s.original
                    for s in parse_string(lib, source)
                    if isinstance(s.original, ast.StatementRule)
                )

            ordinary = rule("p(X) :- q(X).")
            self.assertEqual(
                structured_choice_inputs(ordinary, ordinary.head.literal, []), [0]
            )
            boolean = rule(":- q(X).").head.literal
            self.assertEqual(structured_choice_inputs(ordinary, boolean, []), [])
            for source in ("p(_).", "p(f(_)).", "p(X+1).", "p(f(a;b))."):
                term = rule(source).head.literal.atom.pool[0].arguments[0]
                self.assertIsNone(constructor_variables(term))
            negative = rule("p(f(X),N) :- not q(X).")
            self.assertEqual(
                structured_choice_inputs(negative, negative.head.literal, [1]), []
            )
            unsupported = rule("p(f(X),N) :- q(X+1).")
            self.assertEqual(
                structured_choice_inputs(unsupported, unsupported.head.literal, [1]), []
            )
