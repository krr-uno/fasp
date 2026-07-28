from collections import defaultdict
from collections.abc import Sequence

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.pattern_finders import (
    AggregatePatternFinder,
    InequalityConstraintFinder,
)
from funasp.asp2funasp.transformers.preprocessing import processPipelinetransformers
from funasp.asp2funasp.util.types import FPredicate, FRelation


class FunctionalPredicateFinder:
    """Find functional predicates and relations without retaining program state."""

    def __init__(self, library: Library) -> None:
        self.lib = library

    def find(
        self, statements: Sequence[ast.Statement]
    ) -> tuple[list[FPredicate], list[FRelation]]:
        """Return the functional predicates and relations found in ``statements``.

        Every invocation computes its result from fresh local collections, so a
        finder instance can safely be reused for independent programs.
        """
        processed_statements = list(processPipelinetransformers(self.lib, statements))

        # Clingo rewriting can replace variables with projections (``*``).
        # ``identify_invariant_positions`` treats projections as variant so
        # they are not incorrectly included among a predicate's arguments.
        inequality_finder = InequalityConstraintFinder(self.lib)
        functional_predicates = list(
            inequality_finder.identifyInequalityPattern(processed_statements)
        )

        aggregate_finder = AggregatePatternFinder(self.lib)
        aggregate_finder.identifyAggregatePattern(processed_statements)
        aggregate_finder.identifyCountConstraintPattern(processed_statements)
        functional_predicates.extend(aggregate_finder.getFunctionalPredicates())

        return functional_predicates, self._functional_relations(functional_predicates)

    @staticmethod
    def _functional_relations(
        functional_predicates: Sequence[FPredicate],
    ) -> list[FRelation]:
        """Build complete functional relations from the detected predicates."""
        grouped_predicates = defaultdict(list)
        for functional_predicate in functional_predicates:
            if functional_predicate.condition == []:
                key = (
                    functional_predicate.name,
                    functional_predicate.arguments,
                    functional_predicate.arity,
                )
                grouped_predicates[key].append(functional_predicate.values)

        functional_relations: list[FRelation] = []
        for (name, arguments, arity), value_groups in grouped_predicates.items():
            remaining_indices = set(range(arity)) - set(arguments)
            extracted_values: set[int] = set()
            values: list[tuple[int, ...]] = []
            for value_group in value_groups:
                values.append(tuple(value_group))
                extracted_values.update(value_group)
            if extracted_values == remaining_indices:
                functional_relations.append(FRelation(name, arity, arguments, values))
        return functional_relations
