"""Source conversion and answer-set checks for aggregate-result functions."""

import unittest

from funasp.asp2funasp.util.types import FRelation
from funasp.ast import parse_string
from funasp.core import Library
from tests.asp2funasp.util import ConversionTestCase


class AggregateResultConversionTest(ConversionTestCase):
    def test_choice_aggregate_result_and_lookup(self) -> None:
        _, relations = self.assertConversionEqual(
            """
            { total(N) } :- N = #count { X : item(X) }.
            seen(N) :- total(N).
            #show total/1.
            """,
            """
            #program base.
            { total := N } :- N = #count { X: item(X) }.
            seen(N) :- total=N.
            #showf total/0.
            """,
        )
        self.assertEqual(relations, (FRelation("total", 1, (), [(0,)]),))

    def test_repeated_aggregate_result_and_lookup(self) -> None:
        _, relations = self.assertConversionEqual(
            """
            total(N,N) :- N = #count { X : item(X) }.
            seen(A,B) :- total(A,B).
            #show total/2.
            """,
            """
            #program base.
            total := (N,N) :- N = #count { X: item(X) }.
            seen(A,B) :- total=(A,B).
            #showf total/0.
            """,
        )
        self.assertEqual(relations, (FRelation("total", 2, (), [(0, 1)]),))

    def test_constant_aggregate_input_and_lookup(self) -> None:
        _, relations = self.assertConversionEqual(
            """
            total(1,N) :- N = #count { X : item(X) }.
            seen(K,N) :- total(K,N).
            #show total/2.
            """,
            """
            #program base.
            total(1) := N :- N = #count { X: item(X) }.
            seen(K,N) :- total(K)=N.
            #showf total/1.
            """,
        )
        self.assertEqual(relations, (FRelation("total", 2, (0,), [(1,)]),))

    def test_new_head_forms_preserve_answer_sets(self) -> None:
        for head, lookup, expected_count in (
            ("{ total(N) }", "total(N)", 8),
            ("{ total(N) : N>0 }", "total(N)", 7),
            ("total(N,N)", "total(N,N)", 4),
            ("total(1,N)", "total(1,N)", 4),
        ):
            with self.subTest(head=head):
                source = f"""
                    {{ item(a); item(b) }}.
                    {head} :- N = #count {{ X : item(X) }}.
                    seen(N) :- {lookup}.
                    missing :- not seen(0).
                    #show item/1.
                    #show seen/1.
                    #show missing/0.
                """
                converted, relations = self._convert(source)
                self.assertEqual(len(relations), 1)
                expected = self._models(source)
                self.assertEqual(len(expected), expected_count)
                self.assertEqual(self._models(source, True, prefix="G"), expected)
                self.assertEqual(self._models(converted), expected)

    def test_pooled_results_preserve_both_unary_values(self) -> None:
        source = """
            item(a).
            total(N;N+1) :- N = #count { X : item(X) }.
            #show total/1.
        """
        _, relations = self.assertConversionEqual(
            source,
            """
            #program base.
            item(a).
            total(N;N+1) :- N = #count { X: item(X) }.
            #show total/1. [true]
            """,
        )
        self.assertEqual(relations, ())
        expected = {"total(1) total(2)"}
        self.assertEqual(self._models(source), expected)
        self.assertEqual(self._models(source, True), expected)

    def test_count_assignment_lookups_and_show_signature(self) -> None:
        converted, relations = self.assertConversionEqual(
            """
            num_edges(N) :- N = #count { X,Y : edge(X,Y) }.
            num(0).
            num(N) :- num(N1), N=N1+1, num_edges(E), N<=E.
            missing :- not num_edges(0).
            #show num_edges/1.
            """,
            """
            #program base.
            num_edges := N :- N = #count { X,Y: edge(X,Y) }.
            num(0).
            num(N) :- num(N1); N=N1+1; num_edges=E; N<=E.
            missing :- not num_edges=0.
            #showf num_edges/0.
            """,
        )
        self.assertEqual(relations, (FRelation("num_edges", 1, (), [(0,)]),))
        with Library() as library:
            self.assertTrue(parse_string(library, converted))

    def test_nonfinal_output_and_aggregate_conditions(self) -> None:
        converted, relations = self.assertConversionEqual(
            """
            total(S,K) :- group(K), allowed(S), S = #sum { W,I : item(K,I,W) }.
            seen(N) :- N = #count { K,S : total(S,K) }.
            """,
            """
            #program base.
            total(K) := S :- group(K); allowed(S); S = #sum { W,I: item(K,I,W) }.
            seen := N :- N = #count { K,S: total(K)=S }.
            """,
        )
        self.assertEqual(
            relations,
            (FRelation("total", 2, (1,), [(0,)]), FRelation("seen", 1, (), [(0,)])),
        )

    def test_existing_uniqueness_detection_is_not_duplicated(self) -> None:
        _, relations = self.assertConversionEqual(
            """
            total(K,N) :- key(K), N = #count { I : item(K,I) }.
            :- total(K,N), total(K,M), N != M.
            """,
            """
            #program base.
            total(K) := N :- key(K); N = #count { I: item(K,I) }.
             :- total(K)=N; total(K)=M; N!=M.
            """,
        )
        self.assertEqual(relations, (FRelation("total", 2, (0,), [(1,)]),))

    def test_unfixed_context_is_left_relational_by_full_pipeline(self) -> None:
        source = """
            group(a;b).
            item(a,1).
            item(b,1).
            item(b,2).
            total(N) :- group(K), N = #count { I : item(K,I) }.
            #show total/1.
        """
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
                source = f"""
                    {{ item(a,-2); item(b,3); item(c,3) }}.
                    total(V) :- V = #{operator} {{ W,I : item(I,W) }}.
                    #show.
                    #show selected(I,W) : item(I,W).
                    #show result(V) : total(V).
                """
                converted, relations = self._convert(source)
                self.assertEqual(relations, (FRelation("total", 1, (), [(0,)]),))
                expected = self._models(source, False)
                self.assertEqual(len(expected), 8)
                self.assertEqual(self._models(source, True, prefix="G"), expected)
                self.assertEqual(self._models(converted, False), expected)

    def test_filter_preserves_undefinedness_and_negation(self) -> None:
        source = """
            { item(1); item(2) }.
            allowed(1).
            total(N) :- allowed(N), N = #count { I : item(I) }.
            missing :- not total(1).
            #show.
            #show result(N) : total(N).
            #show absent : missing.
        """
        expected = self._models(source, False)
        self.assertEqual(expected, {"result(1)", "absent"})
        self.assertEqual(self._models(source, True), expected)

    def test_graceful_number_domain_preserves_answer_sets(self) -> None:
        source = """
            edge(a,b).
            edge(b,c).
            num_edges(N) :- N = #count { X,Y : edge(X,Y) }.
            num(0).
            num(N) :- num(N1), N=N1+1, num_edges(E), N<=E.
            #show num/1.
        """
        expected = self._models(source, False)
        self.assertEqual(expected, {"num(0) num(1) num(2)"})
        self.assertEqual(self._models(source, True), expected)


if __name__ == "__main__":
    unittest.main()
