"""
Orchestration of the rewriting pipeline over the prefixed representation.

The ``clingo_funasp`` parser desugars FASP assignments at parse time into
plain clingo AST with prefixed atoms (``a := 1`` becomes ``Fa(1)``). This
pipeline turns that purely syntactic encoding into a semantically correct
ASP program, mirroring the old FASP-node pipeline:

1. Per statement: rename parser prefixes to the configured prefix, rewrite
   ``#some`` assignments, normalize aggregate assignments, rewrite negated
   body literals, and collect evaluable function signatures.
2. Per statement: unnest evaluable functions, rewrite functional equalities
   into prefixed literals, run clingo's statement rewriting, and restore the
   prefixed literals whose unpooled arity is not evaluable.
3. Append the functionality constraints.
"""

from typing import Iterable

from clingo_funasp import ast
from clingo_funasp.core import Location, Position

from funasp.rewriting._context import RewriteContext
from funasp.rewriting.aggregates import normalize_assignment_aggregates
from funasp.rewriting.collectors import collect_evaluable_function_signatures
from funasp.rewriting.comparisons import prefix_comparisons
from funasp.rewriting.constraints import functional_constraints
from funasp.rewriting.negated_literals import rewrite_negate_body_literals
from funasp.rewriting.prefixes import rename_prefixes
from funasp.rewriting.restore import restore_non_evaluable_functions
from funasp.rewriting.some_assignments import rewrite_some_assignments
from funasp.rewriting.unnesting import unnest_statement


def _fix_statement_location(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """
    Work around a clingo-funasp parser bug (present in 6.0.0.post11) where
    statements with assignments lose the file of their begin position,
    producing malformed locations in error messages.
    """
    location = statement.location
    if location.begin.file or not location.end.file:
        return statement
    library = context.lib.library
    begin = Position(
        library, location.end.file, location.begin.line, location.begin.column
    )
    return statement.update(library, location=Location(begin, location.end))


def _clingo_rewrite(
    context: RewriteContext, original: ast.Statement, statement: ast.Statement
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
    statements: Iterable[ast.Statement],
) -> list[ast.Statement]:
    """
    Run the pipeline over parsed statements and return transformed statements.
    """
    pass1: list[tuple[ast.Statement, ast.Statement]] = []
    for original in statements:
        original = _fix_statement_location(context, original)
        stmt = rename_prefixes(context, original)
        stmt = rewrite_some_assignments(context, stmt)
        stmt = normalize_assignment_aggregates(context, stmt)
        stmt = rewrite_negate_body_literals(context, stmt)
        context.evaluable_functions |= collect_evaluable_function_signatures(
            context, stmt
        )
        pass1.append((original, stmt))

    result: list[ast.Statement] = []
    for original, stmt in pass1:
        stmt = unnest_statement(context, stmt)
        stmt = prefix_comparisons(context, stmt)
        result.extend(
            restore_non_evaluable_functions(context, rewritten)
            for rewritten in _clingo_rewrite(context, original, stmt)
        )
    result.extend(functional_constraints(context))
    return result
