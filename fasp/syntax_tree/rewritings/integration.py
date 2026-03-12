from enum import IntEnum, auto
from typing import Iterable

from clingo import ast

from fasp.syntax_tree._context import RewriteContext as FASPRewriteContext
from fasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)
from fasp.syntax_tree.collectors import (
    collect_evaluable_functions,
)
from fasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
from fasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negated_body_literals_from_statements,
)
from fasp.syntax_tree.rewritings.protecting import (
    protect_assignments,
    protect_comparisons,
    restore_assignments,
    restore_comparisons,
)
from fasp.syntax_tree.rewritings.showf import rewrite_showf
from fasp.syntax_tree.rewritings.some_assignments import (
    rewrite_some_choices,
)
from fasp.syntax_tree.rewritings.to_asp import (
    NormalForm2PredicateTransformer,
    functional_constraints,
    to_asp,
)
from fasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer
from fasp.syntax_tree.types import SymbolSignature


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
    context: FASPRewriteContext, statements: Iterable[ast.Statement]
) -> Iterable[ast.Statement]:
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
            for new_stmt in rewritten_list:
                out.append(new_stmt)
        else:
            out.append(stmt)
    context.lib.ignore_info = False
    if errors:
        raise RuntimeError("rewriting failed", errors)
    return out


def transform_to_clingo_statements(
    context: FASPRewriteContext,
    statements: Iterable[FASP_Statement],
) -> list[ast.Statement]:
    """
    Parse the program string, collect variables,
    then run the pipeline and return transformed statements.
    """
    library = context.lib.library
    context.ctx
    statements = [rewrite_showf(context, stmt) for stmt in statements]
    statements = [rewrite_some_choices(library, stmt) for stmt in statements]
    statements = [normalize_assignment_aggregates(library, stmt) for stmt in statements]
    statements = protect_assignments(context, statements)
    statements = protect_comparisons(library, statements)
    statements = _clingo_rewrite_wrapper(context, statements)
    statements = restore_comparisons(library, statements)
    statements = restore_assignments(
        context.lib,
        statements,
        context.prefix_function,
    )
    statements = rewrite_negated_body_literals_from_statements(context.lib, statements)
    evaluable_functions = collect_evaluable_functions(statements)
    transformer = RuleRewriteTransformer(library, evaluable_functions)
    statements = [transformer.transform_rule(stmt) or stmt for stmt in statements]
    statements = to_asp(
        library, statements, evaluable_functions, context.prefix_function
    )
    statements.extend(
        functional_constraints(
            context.lib.library, evaluable_functions, context.prefix_function
        )
    )
    return statements
