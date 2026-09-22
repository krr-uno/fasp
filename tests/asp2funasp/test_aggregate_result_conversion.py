"""Source conversion and answer-set checks for aggregate-result functions."""

import unittest

from funasp.asp2funasp.util.types import FRelation
from funasp.ast import parse_string
from funasp.core import Library
from tests.asp2funasp.util import ConversionTestCase


class AggregateResultConversionTest(ConversionTestCase):
    def test_count_assignment_lookups_and_show_signature(self) -> None:
        converted, relations = self.assertConversionEqual(
            "num_edges(N) :- N = #count { X,Y : edge(X,Y) }. "
            "num(0)."
            "num(N) :- num(N1), N=N1+1, num_edges(E), N<=E. "
            "missing :- not num_edges(0). #show num_edges/1.",
            "#program base.\n"
            "num_edges := N :- N = #count { X,Y: edge(X,Y) }.\n"
            "num(0).\n"
            "num(N) :- num(N1); N=N1+1; num_edges=E; N<=E.\n"
            "missing :- not num_edges=0.\n"
            "#showf num_edges/0.",
        )
        self.assertEqual(relations, (FRelation("num_edges", 1, (), [(0,)]),))
        with Library() as library:
            self.assertTrue(parse_string(library, converted))

    def test_nonfinal_output_and_aggregate_conditions(self) -> None:
        converted, relations = self.assertConversionEqual(
            "total(S,K) :- group(K), allowed(S), S = #sum { W,I : item(K,I,W) }. "
            "seen(N) :- N = #count { K,S : total(S,K) }.",
            "#program base.\n"
            "total(K) := S :- group(K); allowed(S); S = #sum { W,I: item(K,I,W) }.\n"
            "seen := N :- N = #count { K,S: total(K)=S }.",
        )
        self.assertEqual(
            relations,
            (FRelation("total", 2, (1,), [(0,)]), FRelation("seen", 1, (), [(0,)])),
        )

    def test_existing_uniqueness_detection_is_not_duplicated(self) -> None:
        converted, relations = self._convert(
            "total(K,N) :- key(K), N = #count { I : item(K,I) }. "
            ":- total(K,N), total(K,M), N != M."
        )
        # assertConversionEqual
        self.assertEqual(relations, (FRelation("total", 2, (0,), [(1,)]),))
        self.assertIn("total(K) := N", converted)

    def test_unfixed_context_is_left_relational_by_full_pipeline(self) -> None:
        source = (
            "group(a;b). item(a,1). item(b,1). item(b,2). "
            "total(N) :- group(K), N = #count { I : item(K,I) }. "
            "#show total/1."
        )
        converted, relations = self._convert(source)
        self.assertEqual(relations, ())
        self.assertNotIn(":=", converted)
        expected = self._models(source, False)
        self.assertEqual(expected, {"total(1) total(2)"})
        self.assertEqual(self._models(source, True), expected)

    def test_all_operators_preserve_projected_answer_sets_and_empty_results(
        self,
    ) -> None:
        for operator in ("count", "sum", "sum+", "min", "max"):
            with self.subTest(operator=operator):
                # All subsets, including empty, negative, and duplicate weights.
                # Keep the item id in the tuple to preserve aggregate identity.
                source = (
                    "{ item(a,-2); item(b,3); item(c,3) }. "
                    f"total(V) :- V = #{operator} {{ W,I : item(I,W) }}. "
                    "#show. #show selected(I,W) : item(I,W). "
                    "#show result(V) : total(V)."
                )
                converted, relations = self._convert(source)
                self.assertEqual(relations, (FRelation("total", 1, (), [(0,)]),))
                expected = self._models(source, False)
                self.assertEqual(len(expected), 8)
                self.assertEqual(self._models(source, True, prefix="G"), expected)
                self.assertEqual(self._models(converted, False), expected)

    def test_filter_preserves_undefinedness_and_negation(self) -> None:
        source = (
            "{ item(1); item(2) }. allowed(1). "
            "total(N) :- allowed(N), N = #count { I : item(I) }. "
            "missing :- not total(1). #show. "
            "#show result(N) : total(N). #show absent : missing."
        )
        expected = self._models(source, False)
        self.assertEqual(expected, {"result(1)", "absent"})
        self.assertEqual(self._models(source, True), expected)

    def test_graceful_number_domain_preserves_answer_sets(self) -> None:
        source = (
            "edge(a,b). edge(b,c). "
            "num_edges(N) :- N = #count { X,Y : edge(X,Y) }. "
            "num(0). num(N) :- num(N1), N=N1+1, num_edges(E), N<=E. "
            "#show num/1."
        )
        expected = self._models(source, False)
        self.assertEqual(expected, {"num(0) num(1) num(2)"})
        self.assertEqual(self._models(source, True), expected)


if __name__ == "__main__":
    unittest.main()
