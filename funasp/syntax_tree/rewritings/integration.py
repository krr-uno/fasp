from enum import IntEnum, auto
from typing import Iterable

from clingo import ast

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)
from funasp.syntax_tree.collectors import (
    collect_evaluable_functions,
)
from funasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
from funasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negated_body_literals_from_statements,
)
from funasp.syntax_tree.rewritings.protecting import (
    protect_assignment,
    protect_comparisons,
    restore_assignments,
    restore_comparisons,
)
from funasp.syntax_tree.rewritings.showf import rewrite_showf
from funasp.syntax_tree.rewritings.some_assignments import (
    rewrite_some_choices,
)
from funasp.syntax_tree.rewritings.to_asp import (
    NormalForm2PredicateTransformer,
    functional_constraints,
    to_asp,
)
from funasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer
from funasp.syntax_tree.types import SymbolSignature


class PipelineStage(IntEnum):
    SHOWF = auto()
    REWRITE_CHOICE_SOME = auto()
    NORMALIZE_ASSIGNMENT_AGGREGATES = auto()
    PROTECT_ASSIGNMENTS = auto()
    PROTECT_COMPARISONS = auto()
    CLINGO_REWRITE = auto()
    RESTORE_COMPARISONS = auto()
    RESTORE_ASSIGNMENTS = auto()
    NEGATED_LITERALS = auto()
    UNNEST_FUNCTIONS = auto()
    TO_ASP = auto()


# class Statement:

#     def __init__(self, original: FASP_Statement):
#         self.original = original
#         self.has_assignments = isinstance(original, AssignmentRule)
#         self.rewritten: list[FASP_Statement] = []


def _clingo_rewrite_wrapper(
    context: RewriteContext, statements: Iterable[ast.Statement]
) -> list[ast.Statement]:
    """
    Wrapper for clingo's statement rewriting to handle errors.
    """
    ctx = context.ctx
    context.lib.ignore_info = True
    out = []
    errors = []
    for stmt in statements:
        assert not isinstance(stmt, AssignmentRule)
        try:
            rewritten_list = ast.rewrite_statement(ctx, stmt)
        except RuntimeError as e:
            errors.append((stmt, e))
            continue
        if rewritten_list:
            out.extend(rewritten_list)
        else:
            out.append(stmt)
    context.lib.ignore_info = False
    if errors:
        raise RuntimeError("rewriting failed", errors)
    return out


def transform_to_clingo_statements(
    context: RewriteContext,
    statements: Iterable[FASP_Statement],
) -> list[ast.Statement]:
    """
    Parse the program string, collect variables,
    then run the pipeline and return transformed statements.
    """
    library = context.lib.library
    context.ctx
    new_statements: list[ast.Statement] = []
    for stmt in statements:
        new_stmt = rewrite_showf(context, stmt)
        new_stmt = rewrite_some_choices(library, new_stmt)
        new_stmt = normalize_assignment_aggregates(library, new_stmt)
        new_stmt = protect_assignment(context, new_stmt)
        new_stmt = protect_comparisons(context, new_stmt)
        new_statements.append(new_stmt)
        # new_stmts = ast.rewrite_statement(context.ctx, new_stmt)
        # new_statements.extend(new_stmts)

    new_statements = _clingo_rewrite_wrapper(context, new_statements)
    # print(new_statements)
    new_statements = restore_comparisons(context, new_statements)
    new_statements2 = restore_assignments(
        context.lib,
        new_statements,
        context.prefix_function,
    )
    new_statements2 = rewrite_negated_body_literals_from_statements(
        context.lib, new_statements2
    )
    evaluable_functions = collect_evaluable_functions(new_statements2)
    transformer = RuleRewriteTransformer(library, evaluable_functions)
    new_statements2 = [
        transformer.transform_rule(stmt) or stmt for stmt in new_statements2
    ]
    new_statements2 = to_asp(
        library, new_statements2, evaluable_functions, context.prefix_function
    )
    new_statements2.extend(
        functional_constraints(
            context.lib.library, evaluable_functions, context.prefix_function
        )
    )
    return new_statements2
