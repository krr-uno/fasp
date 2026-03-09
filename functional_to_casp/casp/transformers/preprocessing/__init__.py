from .aggregate_head_body_condition_rewrite import AggregateHeadBodyConditionTransformer
from .choice_rule_guard_normalize_rewrite import ChoiceGuardTransformer
from .negated_comparison_head_to_body_rewrite import (
    NegatedComparisonHeadToBodyTransformer,
)
from .notaggregate_constraint_rewrite import NotAggregateConstraintTransformer

__all__ = [
    "AggregateHeadBodyConditionTransformer",
    "NegatedComparisonHeadToBodyTransformer",
    "ChoiceGuardTransformer",
    "NotAggregateConstraintTransformer",
]

from typing import Iterable, List

from clingo.core import Library

import casp.util.util as util
from casp.transformers.preprocessing.base import PreprocessingTransformer
from casp.util.ast import StatementAST


def processPipelinetransformers(
    lib: Library, statements: Iterable[StatementAST]
) -> Iterable[StatementAST]:
    transformers: List[PreprocessingTransformer] = [
        NegatedComparisonHeadToBodyTransformer(lib),
        ChoiceGuardTransformer(lib),
        NotAggregateConstraintTransformer(lib),
        AggregateHeadBodyConditionTransformer(lib),
    ]
    # Split rules with multiple aggregate elements
    initial_asts: List[StatementAST] = []
    for ast_node in statements:
        initial_asts.extend(util.split_multiple_aggregate_elements(lib, ast_node))
    current_asts = initial_asts
    for tr in transformers:
        next_asts = []
        for stmt in current_asts:
            out = tr.rewrite_rule(stmt)
            next_asts.append(out or stmt)

        current_asts = next_asts
    return current_asts
