"""Tuple-valued conversion, source round trips, and answer-set preservation."""

import unittest

from funasp.asp2funasp import convert_statements
from funasp.ast import ast_to_str, parse_string
from funasp.control import Control
from funasp.core import Library


class TupleConversionTest(unittest.TestCase):
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
            return {str(model) for model in control.solve()}

    def test_choice_and_later_occurrences(self):
        source = (
            "1 <= { selectDir(D,O,T): dir(D,O) } <= 1 :- step(T). "
            "go(D,O,T) :- selectDir(D,O,T). "
            "missing(T) :- step(T), not selectDir(left,horizontal,T). "
            "n(N) :- N = #count { D,O,T : selectDir(D,O,T) }. "
            "#show selectDir/3."
        )
        converted = self.convert(source)
        self.assertIn(
            "1 <= { selectDir(T) := (D,O): dir(D,O) } <= 1 :- step(T).",
            converted,
        )
        self.assertIn("go(D,O,T) :- selectDir(T)=(D,O).", converted)
        self.assertIn("not selectDir(T)=(left,horizontal)", converted)
        self.assertIn("#count { D,O,T: selectDir(T)=(D,O) }", converted)
        self.assertIn("#showf selectDir/1.", converted)
        # The rendered directive must show the actual tuple function signature.
        models = self.models("step(1). dir(left,horizontal). " + converted)
        self.assertEqual(models, {"selectDir(1)=(left,horizontal)"})

    def test_preserves_models_and_empty_candidate_failure(self):
        for facts in ("", "dir(left,horizontal).", "dir(left,horizontal;right,vertical)."):
            with self.subTest(facts=facts):
                source = (
                    "step(1;2). " + facts
                    + " 1 { selectDir(D,O,T): dir(D,O) } 1 :- step(T). "
                    "chosen(D,O,T) :- selectDir(D,O,T). "
                    "missing(T) :- step(T), not selectDir(left,horizontal,T). "
                    "#show chosen/3. #show missing/1."
                )
                expected = self.models(source)
                if not facts:
                    self.assertEqual(expected, set())
                else:
                    self.assertTrue(expected)
                self.assertEqual(self.models(source, True), expected)
                self.assertEqual(self.models(self.convert(source)), expected)

    def test_noncontiguous_outputs_and_three_component_tuple(self):
        source = (
            "{ pick(A,K,B,C) : option(A,B,C) } = 1 :- key(K). "
            "seen(A,K,B,C) :- pick(A,K,B,C). #show pick/4."
        )
        converted = self.convert(source)
        self.assertIn("pick(K) := (A,B,C)", converted)
        self.assertIn("pick(K)=(A,B,C)", converted)
        self.assertIn("#showf pick/1.", converted)
        self.assertEqual(
            self.models("key(k). option(a,b,c). " + converted),
            {"pick(k)=(a,b,c)"},
        )

    def test_scalar_tuple_and_name_collision(self):
        source = (
            "{ pick(A,B) : option(A,B) } = 1. "
            "seen(A,B) :- pick(A,B). pick. #show pick/2."
        )
        converted = self.convert(source)
        self.assertIn("pick_1 := (A,B)", converted)
        self.assertIn("pick_1=(A,B)", converted)
        self.assertIn("#showf pick_1/0.", converted)
        self.assertEqual(self.models("option(a,b). " + converted), {"pick_1=(a,b)"})
