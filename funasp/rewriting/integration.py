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
    wrappers = list(statements)
    pass1: list[tuple[Statement, ast.Statement]] = []
    for wrapper in wrappers:
        for stmt in rewrite_some_assignments(context, wrapper.original):
            stmt = normalize_assignment_aggregates(context, stmt)
            stmt = rewrite_negated_body_literals(context, stmt)
            context.evaluable_functions |= collect_evaluable_function_signatures(
                context, stmt
            )
            pass1.append((wrapper, stmt))

    for wrapper in wrappers:
        wrapper.rewritten = []
    for wrapper, stmt in pass1:
        stmt = unnest_statement(context, stmt)
        stmt = rename_prefixes(context, stmt)
        stmt = prefix_comparisons(context, stmt)
        wrapper.rewritten.extend(
            restore_non_evaluable_functions(context, rewritten)
            for rewritten in _clingo_rewrite(context, wrapper, stmt)
        )

    for constraint in functional_constraints(context):
        wrappers.append(Statement(context.lib.library, constraint))
    return wrappers
