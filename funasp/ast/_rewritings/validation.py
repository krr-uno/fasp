"""Validation of intensional-function occurrences in unsupported AST positions."""

from typing import Any

from clingo_funasp import ast

from funasp.ast._rewritings.context import RewriteContext
from funasp.util.ast import (
    RewritingException,
    SemanticError,
    function_arguments_ast,
    is_function,
)
from funasp.util.types import SymbolSignature

_UNSUPPORTED_TERM_STATEMENTS = (
    ast.StatementEdge,
    ast.StatementExternal,
    ast.StatementHeuristic,
    ast.StatementProject,
    ast.StatementShow,
)


def _is_intensional_function(
    context: RewriteContext, term: ast.TermFunction | ast.TermSymbolic
) -> bool:
    name, arguments = function_arguments_ast(context.lib.library, term)
    if isinstance(term, ast.TermFunction):
        arities = {len(entry.arguments) for entry in term.pool}
    else:
        arities = {len(arguments)}
    return any(
        SymbolSignature(name, arity) in context.intensional_functions
        for arity in arities
    )


def _find_rough_intensional_function(
    context: RewriteContext, node: Any
) -> ast.TermFunction | ast.TermSymbolic | None:
    """Find an intensional term that is not an equality's assigned function."""
    found: ast.TermFunction | ast.TermSymbolic | None = None

    def visit(child: Any) -> None:
        nonlocal found
        if found is not None:
            return
        if isinstance(child, ast.LiteralComparison):
            if (
                is_function(child.left)
                and _is_intensional_function(context, child.left)
                and len(child.right) == 1
                and child.right[0].relation == ast.Relation.Equal
            ):
                # The outer function is a supported functional equation. Its
                # arguments can still contain unsupported rough occurrences.
                child.left.visit(visit)
                child.right[0].visit(visit)
                return
        if isinstance(child, ast.TermFunction | ast.TermSymbolic) and (
            _is_intensional_function(context, child)
        ):
            found = child
            return
        child.visit(visit)

    node.visit(visit)
    return found


def validate_intensional_function_positions(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """Reject rough function terms in statement forms without unnesting support."""
    if not isinstance(statement, _UNSUPPORTED_TERM_STATEMENTS):
        return statement
    offending = _find_rough_intensional_function(context, statement)
    if offending is None:
        return statement
    construct = type(statement).__name__.removeprefix("Statement")
    raise RewritingException(
        [
            SemanticError(
                offending.location,
                f"intensional functions are not supported in {construct} statements: "
                f"'{offending}'",
            )
        ]
    )
