from typing import Iterable

from clingo import ast
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    FASP_AST_T,
    AssignmentRule,
    FASP_Statement,
)
from fasp.syntax_tree.until import transform_iterable
from fasp.util.ast import (
    ELibrary,
)


def _rewrite_negated_body_literal(
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
    false_lit = ast.LiteralBoolean(
        library,
        lit.location,
        ast.Sign.NoSign,
        False,
    )

    # Create conditional literal: #false : r(X)
    return ast.BodyConditionalLiteral(
        library,
        lit.location,
        false_lit,
        [lit.update(library, sign=ast.Sign.NoSign)],
    )


def rewrite_negated_body_literals(
    library: Library, statement: FASP_Statement
) -> FASP_Statement:
    if not isinstance(statement, ast.StatementRule | AssignmentRule):
        return statement
    new_body = transform_iterable(
        statement.body, lambda lit: _rewrite_negated_body_literal(library, lit)
    )
    if new_body is None:
        return statement
    return statement.update(library, body=new_body)


def rewrite_negated_body_literals_from_statements(
    library: ELibrary,
    statements: Iterable[FASP_Statement],
) -> Iterable[FASP_Statement]:
    return [rewrite_negated_body_literals(library.library, stmt) for stmt in statements]
