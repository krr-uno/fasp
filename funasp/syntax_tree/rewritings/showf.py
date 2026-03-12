from clingo.ast import StatementShowSignature

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import FASP_Statement, ShowFDirective


def rewrite_showf(ctx: RewriteContext, statement: FASP_Statement) -> FASP_Statement:
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
