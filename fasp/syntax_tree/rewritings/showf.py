from typing import Iterable

from clingo.ast import StatementShowSignature

from fasp.syntax_tree._context import RewriteContext
from fasp.syntax_tree._nodes import FASP_Statement, ShowFDirective


def showf_to_show(ctx: RewriteContext, statement: FASP_Statement) -> FASP_Statement:
    if not isinstance(statement, ShowFDirective):
        return statement
    name = ctx.prefix_function + statement.signature.name
    arity = statement.signature.arity + 1
    return StatementShowSignature(
        ctx.lib.library,
        statement.location,
        name,
        arity,
        sign=False,
        value=True,
    )


def showf_to_show_transformer(
    ctx: RewriteContext, statements: Iterable[FASP_Statement]
) -> Iterable[FASP_Statement]:
    return [showf_to_show(ctx, stmt) for stmt in statements]
