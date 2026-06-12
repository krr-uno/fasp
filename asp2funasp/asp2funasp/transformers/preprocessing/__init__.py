from .aggregate_head_body_condition_rewrite import AggregateHeadBodyConditionTransformer
from .choice_rule_guard_normalize_rewrite import ChoiceGuardTransformer
from .constraint_aggregate_guard_normalization import (
    ConstraintAggregateGuardTransformer,
)
from .negated_comparison_head_to_body_rewrite import (
    NegatedComparisonHeadToBodyTransformer,
)
from .notaggregate_constraint_rewrite import NotAggregateConstraintTransformer

__all__ = [
    "AggregateHeadBodyConditionTransformer",
    "NegatedComparisonHeadToBodyTransformer",
    "ChoiceGuardTransformer",
    "NotAggregateConstraintTransformer",
    "ConstraintAggregateGuardTransformer",
]

from typing import Iterable, List

from clingo import ast
from clingo.ast import RewriteContext, rewrite_statement
from clingo.core import Library

import asp2funasp.util.util as util
from asp2funasp.transformers.preprocessing.base import PreprocessingTransformer


def processPipelinetransformers(
    lib: Library, statements: Iterable[ast.Statement]
) -> Iterable[ast.Statement]:
    transformers: List[PreprocessingTransformer] = [
        NegatedComparisonHeadToBodyTransformer(lib),
        ChoiceGuardTransformer(lib),
        NotAggregateConstraintTransformer(lib),
        AggregateHeadBodyConditionTransformer(lib),
        ConstraintAggregateGuardTransformer(lib),
    ]
    # Split rules with multiple aggregate elements
    initial_asts: List[ast.Statement] = []
    for ast_node in statements:
        initial_asts.extend(util.split_multiple_aggregate_elements(lib, ast_node))
    current_asts = initial_asts

    rewrite_context = RewriteContext(lib)
    current_asts = rewrite_statements_wrapper(rewrite_context, current_asts)

    for tr in transformers:
        next_asts = []
        for stmt in current_asts:
            out = tr.rewrite_rule(stmt)
            next_asts.append(out or stmt)
        current_asts = next_asts
    return current_asts


def rewrite_statements_wrapper(
    context: RewriteContext, statements: List[ast.Statement]
) -> List[ast.Statement]:
    errors = []
    rewritten: List[ast.Statement] = []
    for stmt in statements:
        try:
            out = rewrite_statement(context, stmt)
            rewritten.extend(out)
        except RuntimeError as e:
            errors.append((stmt, e))
    if errors:
        raise RuntimeError("clingo rewriting failed", errors)
    return rewritten
