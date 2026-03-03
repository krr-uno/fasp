from typing import Iterable

from clingo import ast
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)
from fasp.util.ast import ELibrary, transform_iterable


def _rewrite_body_literal(
    library: Library, literal: ast.BodyLiteral
) -> None | ast.BodyConditionalLiteral:
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


def _rewrite_statement(library: Library, statement: FASP_Statement) -> FASP_Statement:
    if not isinstance(statement, ast.StatementRule | AssignmentRule):
        return statement
    new_body = transform_iterable(library, statement.body, _rewrite_body_literal)
    if new_body is None:
        return statement
    return statement.update(library, body=new_body)


def rewrite_negated_body_literals_from_statements(
    library: ELibrary,
    statements: Iterable[FASP_Statement],
) -> Iterable[FASP_Statement]:
    return [_rewrite_statement(library.library, stmt) for stmt in statements]
