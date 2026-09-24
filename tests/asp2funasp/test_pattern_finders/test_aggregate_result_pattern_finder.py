"""Aggregate-result inference, including scope and definition counterexamples."""

import textwrap
import unittest

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.pattern_finders import AggregateResultPatternFinder
from funasp.asp2funasp.util.types import FPredicate


class AggregateResultPatternFinderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.finder = AggregateResultPatternFinder(self.lib)

    def _apply(self, source: str) -> list[FPredicate]:
        """Parse original statements, retaining #program scope declarations."""
        statements: list[ast.Statement] = []
        ast.parse_string(self.lib, textwrap.dedent(source).strip(), statements.append)
        return self.finder.find(statements)

    def assertFPredicateEqual(self, program: str, expected: list[FPredicate]) -> None:
        """Compare detected predicates without depending on their order."""
        self.assertCountEqual(self._apply(program), expected)

    def test_all_aggregate_operators_and_guard_directions(self) -> None:
        for operator in ("count", "sum", "sum+", "min", "max"):
            for equality in (
                f"R = #{operator} {{ V,I : item(K,I,V) }}",
                f"#{operator} {{ V,I : item(K,I,V) }} = R",
            ):
                with self.subTest(equality=equality):
                    self.assertFPredicateEqual(
                        f"total(K,R) :- group(K), {equality}.",
                        [FPredicate("total", 2, (0,), (1,), [])],
                    )

    def test_zero_inputs_local_variables_and_filters(self) -> None:
        self.assertFPredicateEqual(
            "total(N) :- allowed(N), N = #count { X,Y : edge(X,Y); Z : lone(Z) }.",
            [FPredicate("total", 1, (), (0,), [])],
        )

    def test_nonfinal_output_and_multiple_inputs(self) -> None:
        self.assertFPredicateEqual(
            "total(V,K,T) :- key(K,T), V = #sum { W,I : item(K,T,I,W) }.",
            [FPredicate("total", 3, (1, 2), (0,), [])],
        )

    def test_empty_aggregate_and_additional_guard(self) -> None:
        self.assertFPredicateEqual(
            "total(N) :- N = #count {} <= 3.", [FPredicate("total", 1, (), (0,), [])]
        )

    def test_independent_body_variables_are_only_filters(self) -> None:
        self.assertFPredicateEqual(
            "total(N) :- enabled(K), N = #count { X : item(X) }.",
            [FPredicate("total", 1, (), (0,), [])],
        )

    def test_choice_head(self) -> None:
        self.assertFPredicateEqual(
            "{ total(N) } :- N = #count { X : item(X) }.",
            [FPredicate("total", 1, (), (0,), [])],
        )

    def test_repeated_result_forms_tuple_output(self) -> None:
        self.assertFPredicateEqual(
            "total(N,N) :- N = #count { X : item(X) }.",
            [FPredicate("total", 2, (), (0, 1), [])],
        )

    def test_constant_input(self) -> None:
        self.assertFPredicateEqual(
            "total(1,N) :- N = #count { X : item(X) }.",
            [FPredicate("total", 2, (0,), (1,), [])],
        )

    def test_repeated_inputs_and_noncontiguous_outputs(self) -> None:
        self.assertFPredicateEqual(
            "total(N,K,N,K) :- key(K), N = #count { X : item(K,X) }.",
            [FPredicate("total", 4, (1, 3), (0, 2), [])],
        )

    def test_choice_condition_can_filter_the_result(self) -> None:
        self.assertFPredicateEqual(
            "{ total(N) : allowed(N) } :- N = #count { X : item(X) }.",
            [FPredicate("total", 1, (), (0,), [])],
        )

    def test_choice_condition_cannot_supply_hidden_aggregate_inputs(self) -> None:
        self.assertFPredicateEqual(
            "{ total(N) : key(K) } :- N = #count { X : item(K,X) }.",
            [],
        )

    def test_pooled_results_are_not_a_tuple(self) -> None:
        # The pool defines two total/1 atoms, not one total/2 tuple-valued atom.
        self.assertFPredicateEqual("total(N;N+1) :- N = #count { X : item(X) }.", [])

    def test_rejects_unfixed_context_in_conditions_and_tuples(self) -> None:
        programs = [
            "total(N) :- group(K), N = #count { I : item(K,I) }.",
            "total(N) :- weight(W), N = #sum { W,I : item(I) }.",
            "total(N) :- bound(K), N = #count { I : item(K,I) } <= K.",
            "total(N) :- N = #count { I : item(N,I) }.",
            "total(N) :- N = #sum { N,I : item(I) }.",
        ]
        for program in programs:
            with self.subTest(program=program):
                self.assertFPredicateEqual(program, [])

    def test_rejects_nonassignment_aggregates(self) -> None:
        for body in (
            "not N = #count { X : item(X) }",
            "not not N = #count { X : item(X) }",
            "N <= #count { X : item(X) }",
            "N != #count { X : item(X) }",
            "N+1 = #count { X : item(X) }",
            "M = #count { X : item(X) }",
            "#count { X : item(X) }",
            "item(N)",
        ):
            with self.subTest(body=body):
                self.assertFPredicateEqual(f"total(N) :- domain(N), {body}.", [])

    def test_rejects_unsupported_heads(self) -> None:
        for head in (
            "",
            "total",
            "-total(N)",
            "not total(N)",
            "{ total(N); other(N) }",
            "{ not total(N) }",
            "total(N) | other(N)",
            "total(_)",
            "total(key(K),N)",
            "total := N",
        ):
            with self.subTest(head=head):
                self.assertFPredicateEqual(
                    f"{head} :- N = #count {{ X : item(X) }}.", []
                )

    def test_rejects_competing_definitions_and_external_declarations(self) -> None:
        definition = "total(N) :- N = #count { X : item(X) }."
        for other in (
            "total(99).",
            "total(N) :- other(N).",
            "{ total(99) }.",
            "total(99) | alternate.",
            "#external total(99).",
        ):
            for program in (definition + other, other + definition):
                with self.subTest(program=program):
                    self.assertFPredicateEqual(program, [])

    def test_rejects_parameterized_program_parts(self) -> None:
        self.assertFPredicateEqual(
         """#program step(t). 
            total(N) :- N = #count { X : item(t,X) }.
         """, []
        )

    def test_finder_can_be_reused_and_ignores_unrelated_directives(self) -> None:
        self.assertFPredicateEqual(
            """#external other.
            total(N) :- N = #count { X : item(X) }.
            #show total/1.
            """,
            [FPredicate("total", 1, (), (0,), [])],
        )
        self.assertFPredicateEqual("unrelated.", [])


if __name__ == "__main__":
    unittest.main()
