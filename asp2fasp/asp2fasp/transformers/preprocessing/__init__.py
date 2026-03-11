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

from clingo.ast import RewriteContext, rewrite_statement
from clingo.core import Library

import asp2fasp.util.util as util
from asp2fasp.transformers.preprocessing.base import PreprocessingTransformer
from asp2fasp.util.ast import StatementAST


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

    rewrite_context = RewriteContext(lib)
    errors = []
    next_asts: List[StatementAST] = []
    for stmt in current_asts:
        try:
            out = rewrite_statement(rewrite_context, stmt)
            next_asts.extend(out)
        except RuntimeError as e:
            errors.append((stmt, e))
    if errors:
        raise RuntimeError("clingo rewriting failed", errors)
    current_asts = next_asts

    for tr in transformers:
        next_asts = []
        for stmt in current_asts:
            out = tr.rewrite_rule(stmt)
            next_asts.append(out or stmt)
        current_asts = next_asts
    return current_asts
