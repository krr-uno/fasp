"""
Orchestration of the rewriting pipeline over the prefixed representation.

The ``clingo_funasp`` parser desugars FASP assignments at parse time into
plain clingo AST with prefixed atoms (``a := 1`` becomes ``Fa(1)``). This
pipeline turns that purely syntactic encoding into a semantically correct
ASP program, mirroring the old FASP-node pipeline:

1. Per statement: rewrite ``#some`` assignments, normalize aggregate
   assignments, rewrite negated body literals, and collect evaluable function
   signatures.
2. Per statement: unnest evaluable functions, rename parser prefixes to the
   configured prefix, rewrite functional equalities into prefixed literals,
   run clingo's statement rewriting, and restore the prefixed literals whose
   unpooled arity is not evaluable.
3. Append the functionality constraints.
"""

from functools import partial
from typing import Iterable

from clingo_funasp import ast

from funasp.ast import Statement
from funasp.rewriting._context import RewriteContext
from funasp.rewriting.aggregates import normalize_assignment_aggregates
from funasp.rewriting.collectors import collect_evaluable_function_signatures
from funasp.rewriting.comparisons import prefix_comparisons
from funasp.rewriting.constraints import functional_constraints
from funasp.rewriting.negated_literals import rewrite_negated_body_literals
from funasp.rewriting.prefixes import rename_prefixes
from funasp.rewriting.restore import restore_non_evaluable_functions
from funasp.rewriting.some_assignments import rewrite_some_assignments
from funasp.rewriting.unnesting import unnest_statement


def _clingo_rewrite(
    context: RewriteContext, original: Statement, statement: ast.Statement
) -> list[ast.Statement]:
    """
    Wrapper for clingo's statement rewriting to handle errors.
    """
    try:
        context.lib.processing_statement(str(original))
        return list(ast.rewrite_statement(context.ctx, statement))
    except RuntimeError as e:
        raise RuntimeError("rewriting failed", [(statement, e)])
    finally:
        context.lib.clear_processing_statement()


def rewrite_statements(
    context: RewriteContext,
    statements: Iterable[Statement],
) -> list[Statement]:
    """
    Run the pipeline over parsed statements and return transformed statements.

    Each input :class:`~funasp.ast.Statement` keeps its ``original`` and has its
    ``rewritten`` list filled with the clingo statements it expands to. The
    functionality constraints are appended as additional wrapped statements.
    """
    for stmt in statements:
        stmt.rewrite(partial(rewrite_some_assignments, context))
        stmt.rewrite(partial(normalize_assignment_aggregates, context))
        stmt.rewrite(partial(rewrite_negated_body_literals, context))
        for clingo_stmt in stmt.rewritten:
            context.evaluable_functions |= collect_evaluable_function_signatures(
                context, clingo_stmt
            )
    for stmt in statements:
        stmt.rewrite(partial(unnest_statement, context))
        stmt.rewrite(partial(rename_prefixes, context))
        stmt.rewrite(partial(prefix_comparisons, context))
        stmt.rewrite(partial(_clingo_rewrite, context, stmt))
        stmt.rewrite(partial(restore_non_evaluable_functions, context))

    new_statements = list(statements)
    for constraint in functional_constraints(context):
        new_statements.append(Statement(context.lib.library, constraint))
    return new_statements
