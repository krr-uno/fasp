from functools import singledispatchmethod
from typing import Any, Iterable

from clingo import ast
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    FASP_AST_T,
    FASP_Statement,
)
from fasp.util.ast import (
    BodyLiteralAST,
    ELibrary,
    LiteralAST,
)


class RemoveNegatedLiteralsHead:
    def __init__(self) -> None:
        self.lib = Library()
        self.moved_to_body: list[BodyLiteralAST] = []
        self.changed = False

    def _flip_sign(self, lit: LiteralAST) -> LiteralAST:
        if lit.sign == ast.Sign.Single:
            new_sign = ast.Sign.Double
        elif lit.sign == ast.Sign.Double:
            new_sign = ast.Sign.Single
        else:
            return lit  # should not happen
        return lit.update(self.lib, sign=new_sign)

    def transform_statement(self, stmt: FASP_Statement) -> FASP_Statement | None:

        if not isinstance(stmt, ast.StatementRule):
            return None
        update: dict[str, Any] = {}
        new_head = self.visit(stmt.head)

        # new_head = stmt.head
        if not self.changed:
            return None
        update["head"] = new_head

        new_body = list(stmt.body) + self.moved_to_body
        update["body"] = new_body
        return stmt.update(self.lib, **update)

    @singledispatchmethod
    def visit(self, node: FASP_AST_T) -> FASP_AST_T:
        print(f"hi {1}")

        return node

    @visit.register
    def _(self, node: ast.HeadSimpleLiteral) -> ast.HeadSimpleLiteral | None:
        lit = node.literal
        print(f"hi {lit}")

        if lit.sign != ast.Sign.NoSign:
            self.changed = True

            # flip sign and move to body
            flipped = self._flip_sign(lit)
            self.moved_to_body.append(ast.BodySimpleLiteral(self.lib, flipped))

            return None  # remove from head

        return node


def remove_negated_literals_from_head_in_statement(
    statement: FASP_Statement,
) -> FASP_Statement | None:
    transformer = RemoveNegatedLiteralsHead()
    return transformer.transform_statement(statement)


def remove_negated_literals_from_head_in_statements(
    statements: Iterable[FASP_Statement],
) -> Iterable[FASP_Statement]:
    new_statements = []
    for stmt in statements:
        new_stmt = remove_negated_literals_from_head_in_statement(stmt)
        new_statements.append(new_stmt or stmt)
    return new_statements
