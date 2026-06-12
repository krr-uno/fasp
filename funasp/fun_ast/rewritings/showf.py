from clingo_funasp.ast import StatementShowSignature

from funasp.fun_ast._context import RewriteContext
from funasp.fun_ast._nodes import FASP_Statement, ShowFDirective


def rewrite_showf(ctx: RewriteContext, statement: FASP_Statement) -> FASP_Statement:
    """Rewrite a #showf directive into the corresponding prefixed #show signature."""
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
