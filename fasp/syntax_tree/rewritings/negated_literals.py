from functools import singledispatchmethod
from typing import Any, Iterable, List

from clingo import ast
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    FASP_AST_T,
    AssignmentRule,
    FASP_Statement,
)
from fasp.util.ast import (
    BodyLiteralAST,
    ELibrary,
)


class RemoveNegatedLiteralsHead:
    def __init__(
        self,
        library: Library,
    ) -> None:
        self.lib = library
        self.moved_to_body: list[BodyLiteralAST] = []
        self.changed = False

    def _transform_statement_rule[
        T: (
            ast.StatementRule,
            AssignmentRule,
        )
    ](self, stmt: T) -> T | None:
        update: dict[str, Any] = {}

        new_body: List[BodyLiteralAST] = []
        any_changed = False

        for literal in stmt.body:
            new_literal = self.dispatch(literal)
            if new_literal is not None:
                any_changed = True
                new_body.append(new_literal)
            else:
                new_body.append(literal)
        if not any_changed:
            return None
        update["body"] = new_body

        new_body = list(stmt.body) + self.moved_to_body

        return stmt.update(self.lib, **update)

    @singledispatchmethod
    def transform_statement(self, stmt: FASP_Statement) -> FASP_Statement | None:
        return stmt

    @transform_statement.register
    def _(self, stmt: ast.StatementRule) -> ast.StatementRule | None:
        return self._transform_statement_rule(stmt)

    @transform_statement.register
    def _(self, stmt: AssignmentRule) -> AssignmentRule | None:
        return self._transform_statement_rule(stmt)

    @singledispatchmethod
    def dispatch(self, node: FASP_AST_T) -> FASP_AST_T | None:
        return None

    @dispatch.register
    def _(self, node: ast.BodySimpleLiteral) -> ast.BodyConditionalLiteral | None:
        lit = node.literal

        # Only rewrite single-negated literals: not r(X)
        if lit.sign != ast.Sign.Single:
            return None

        # avoid rewriting if: not #false.
        if isinstance(lit, ast.LiteralBoolean):
            return None

        # Build #false
        false_lit = ast.LiteralBoolean(
            self.lib,
            lit.location,
            ast.Sign.NoSign,
            False,
        )

        # Remove negation from original literal
        positive_lit = lit.update(self.lib, sign=ast.Sign.NoSign)

        # Create conditional literal: #false : r(X)
        cond = ast.BodyConditionalLiteral(
            self.lib,
            lit.location,
            false_lit,
            [positive_lit],
        )

        return cond


def rewrite_negated_body_literals(
    library: Library, statement: FASP_Statement
) -> FASP_Statement | None:
    transformer = RemoveNegatedLiteralsHead(library)
    return transformer.transform_statement(statement)


def rewrite_negated_body_literals_from_statements(
    library: ELibrary,
    statements: Iterable[FASP_Statement],
) -> Iterable[FASP_Statement]:
    new_statements = []
    for stmt in statements:
        new_stmt = rewrite_negated_body_literals(library.library, stmt)
        new_statements.append(new_stmt or stmt)
    return new_statements
