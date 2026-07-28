"""Stateless orchestration of ASP-to-FUNASP conversion."""

from collections.abc import Sequence
from dataclasses import dataclass
from enum import StrEnum

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.rewriting import FunctionalPredicateFinder
from funasp.asp2funasp.rewriting.filter_disjunctions import (
    remove_frelations_in_head_disjunctions,
)
from funasp.asp2funasp.rewriting.rewrite_into_funasp import (
    FunctionalPredicateRewriteTransformer,
)
from funasp.asp2funasp.util.types import FPredicate, FRelation, SymbolSignature


class RelationSkipReason(StrEnum):
    """Reasons a detected functional relation is not converted."""

    UNSUPPORTED_OUTPUT_COUNT = (
        "only relations with exactly one output position are supported"
    )
    HEAD_DISJUNCTION = "predicate occurs in a disjunctive head"


@dataclass(frozen=True, slots=True)
class SkippedRelation:
    """A detected functional relation excluded from conversion."""

    relation: FRelation
    reason: RelationSkipReason


@dataclass(frozen=True, slots=True)
class ConversionResult:
    """The converted program and metadata produced during its analysis."""

    converted_statements: tuple[ast.Statement, ...]
    functional_predicates: tuple[FPredicate, ...]
    accepted_relations: tuple[FRelation, ...]
    function_name_mapping: dict[SymbolSignature, str]
    skipped_relations: tuple[SkippedRelation, ...]


def _has_single_output(relation: FRelation) -> bool:
    output_positions = {
        position for value_group in relation.values for position in value_group
    }
    return len(output_positions) == 1


def convert_statements(
    library: Library,
    statements: Sequence[ast.Statement],
) -> ConversionResult:
    """Analyze and convert ``statements`` without retaining cross-call state.

    Preprocessing is performed internally by :class:`FunctionalPredicateFinder`
    only for pattern detection. Rewriting is always applied to the original
    statements supplied by the caller.
    """
    original_statements = tuple(statements)
    functional_predicates, detected_relations = FunctionalPredicateFinder(library).find(
        original_statements
    )

    single_output_relations: list[FRelation] = []
    skipped_relations: list[SkippedRelation] = []
    for relation in detected_relations:
        if _has_single_output(relation):
            single_output_relations.append(relation)
        else:
            skipped_relations.append(
                SkippedRelation(
                    relation,
                    RelationSkipReason.UNSUPPORTED_OUTPUT_COUNT,
                )
            )

    accepted_relations = remove_frelations_in_head_disjunctions(
        library,
        original_statements,
        single_output_relations,
    )
    for relation in single_output_relations:
        if relation not in accepted_relations:
            skipped_relations.append(
                SkippedRelation(
                    relation,
                    RelationSkipReason.HEAD_DISJUNCTION,
                )
            )

    transformer = FunctionalPredicateRewriteTransformer.from_program(
        library,
        accepted_relations,
        original_statements,
    )
    converted_statements: list[ast.Statement] = []
    for statement in original_statements:
        converted = transformer.transform_rule(statement)
        if converted is None:
            converted_statements.append(statement)
        else:
            assert isinstance(converted, ast.Statement)
            converted_statements.append(converted)

    return ConversionResult(
        converted_statements=tuple(converted_statements),
        functional_predicates=tuple(functional_predicates),
        accepted_relations=tuple(accepted_relations),
        function_name_mapping=dict(transformer.function_name_index),
        skipped_relations=tuple(skipped_relations),
    )
