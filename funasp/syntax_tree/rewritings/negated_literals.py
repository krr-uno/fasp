from typing import Iterable

from clingo import ast
from clingo.core import Library

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)
from funasp.util.ast import transform_iterable


def _rewrite_body_literal(
    library: Library, literal: ast.BodyLiteral
) -> None | ast.BodyConditionalLiteral:
    """Rewrite a negated body literal into an equivalent conditional literal when needed."""
    if (
        not isinstance(literal, ast.BodySimpleLiteral)
        or isinstance(literal.literal, ast.LiteralBoolean)
        or literal.literal.sign != ast.Sign.Single
    ):
        return None

    lit = literal.literal

    # Build #false
    false_lit = ast.LiteralBoolean(library, lit.location, ast.Sign.NoSign, False)

    # Create conditional literal: #false : r(X)
    return ast.BodyConditionalLiteral(
        library, lit.location, false_lit, [lit.update(library, sign=ast.Sign.NoSign)]
    )


def rewrite_negate_body_literals(
    context: RewriteContext, statement: FASP_Statement
) -> FASP_Statement:
    """Rewrite eligible negated body literals inside a single statement."""
    if not isinstance(statement, ast.StatementRule | AssignmentRule):
        return statement
    new_body = transform_iterable(
        context.lib.library, statement.body, _rewrite_body_literal
    )
    if new_body is None:
        return statement
    return statement.update(context.lib.library, body=new_body)


def rewrite_negated_body_literals_from_statements(
    context: RewriteContext,
    statements: Iterable[FASP_Statement],
) -> Iterable[FASP_Statement]:
    """Rewrite eligible negated body literals across a sequence of statements."""
    return [rewrite_negate_body_literals(context, stmt) for stmt in statements]
