from collections.abc import Sequence

from clingo import ast
from clingo.core import Library

from fasp.parsing.lexer import UnParsedAssignmentRule, UnParsedClingoCode
from fasp.util.ast import StatementAST, TermAST, parse_body


def _parse_clingo_block(
    library: Library, block: UnParsedClingoCode
) -> list[StatementAST]:
    statements = []
    ast.parse_string(ast.Library(), block.source, statements.append)
    return statements


def _parse_assignment_right(library: Library, code: str) -> tuple[
    TermAST | ast.BodyAggregate,
    Sequence[ast.StatementComment],
    Sequence[ast.StatementComment],
]:
    try:
        body, pc, lt = parse_body(library, code)
    except Exception:
        raise ValueError(
            f"Expected exactly one term or aggregate in assignment right-hand side of an assignment, found {code}"
        )
    if len(body) != 1:
        raise ValueError(
            f"Expected exactly one term or aggregate in assignment right-hand side of an assignment, found {code}"
        )
    right = body[0]
    if isinstance(right, ast.BodySimpleLiteral) and isinstance(
        right.literal, ast.LiteralSymbolic
    ):
        right = right.literal.atom
    elif (
        not isinstance(right, ast.BodyAggregate)
        or right.left is not None
        or right.right is not None
    ):
        raise ValueError(
            f"Expected exactly one term or aggregate in assignment right-hand side of an assignment, found {code}"
        )
    return right, pc, lt


# def _parse_assignment_head(library: Library, code: str, assignment_pos: int) -> StatementAST:


# def _parse_assignment_rule(library: Library, rule: UnParsedAssignmentRule) -> StatementAST:
#     statement = StatementAST()
#     ast.parse_string(ast.Library(), rule.source, statement.append)
#     return statement
