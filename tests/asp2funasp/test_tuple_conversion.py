"""Tuple-valued conversion, source round trips, and answer-set preservation."""

import unittest

from tests.asp2funasp.util import ConversionTestCase


class TupleConversionTest(ConversionTestCase):

    def test_choice_and_later_occurrences(self) -> None:
        source = """
            1 <= { selectDir(D,O,T): dir(D,O) } <= 1 :- step(T).
            go(D,O,T) :- selectDir(D,O,T).
            missing(T) :- step(T), not selectDir(left,horizontal,T).
            n(N) :- N = #count { D,O,T : selectDir(D,O,T) }.
            #show selectDir/3.
        """
        converted, _ = self.assertConversionEqual(
            source,
            """
            #program base.
            1 <= { selectDir(T) := (D,O): dir(D,O) } <= 1 :- step(T).
            go(D,O,T) :- selectDir(T)=(D,O).
            missing(T) :- step(T); not selectDir(T)=(left,horizontal).
            n := N :- N = #count { D,O,T: selectDir(T)=(D,O) }.
            #showf selectDir/1.
            """,
        )
        # The rendered directive must show the actual tuple function signature.
        models = self._models("step(1). dir(left,horizontal). " + converted)
        self.assertEqual(models, {"selectDir(1)=(left,horizontal)"})

    def test_preserves_models_and_empty_candidate_failure(self) -> None:
        for facts in (
            "",
            "dir(left,horizontal).",
            "dir(left,horizontal;right,vertical).",
        ):
            with self.subTest(facts=facts):
                source = f"""
                    step(1;2).
                    {facts}
                    1 {{ selectDir(D,O,T): dir(D,O) }} 1 :- step(T).
                    chosen(D,O,T) :- selectDir(D,O,T).
                    missing(T) :- step(T), not selectDir(left,horizontal,T).
                    #show chosen/3.
                    #show missing/1.
                """
                expected = self._models(source)
                if not facts:
                    self.assertEqual(expected, set())
                else:
                    self.assertTrue(expected)
                self.assertEqual(self._models(source, True), expected)
                self.assertEqual(self._models(self._convert_source(source)), expected)

    def test_noncontiguous_outputs_and_three_component_tuple(self) -> None:
        source = """
            { pick(A,K,B,C) : option(A,B,C) } = 1 :- key(K).
            seen(A,K,B,C) :- pick(A,K,B,C).
            #show pick/4.
        """
        converted, _ = self.assertConversionEqual(
            source,
            """
            #program base.
            { pick(K) := (A,B,C): option(A,B,C) } = 1 :- key(K).
            seen(A,K,B,C) :- pick(K)=(A,B,C).
            #showf pick/1.
            """,
        )
        self.assertEqual(
            self._models("key(k). option(a,b,c). " + converted),
            {"pick(k)=(a,b,c)"},
        )

    def test_scalar_tuple_and_name_collision(self) -> None:
        source = """
            { pick(A,B) : option(A,B) } = 1.
            seen(A,B) :- pick(A,B).
            pick.
            #show pick/2.
        """
        converted, _ = self.assertConversionEqual(
            source,
            """
            #program base.
            { pick_1 := (A,B): option(A,B) } = 1.
            seen(A,B) :- pick_1=(A,B).
            pick.
            #showf pick_1/0.
            """,
        )
        self.assertEqual(self._models("option(a,b). " + converted), {"pick_1=(a,b)"})


if __name__ == "__main__":
    unittest.main()
