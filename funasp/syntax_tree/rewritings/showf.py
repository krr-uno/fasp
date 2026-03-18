from clingo.ast import StatementShowSignature

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import FASP_Statement, ShowFDirective
from funasp.syntax_tree.rewritings.unnesting._statement import Statement


def _rewrite_showf(ctx: RewriteContext, statement: FASP_Statement) -> FASP_Statement:
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


def rewrite_showf(ctx: RewriteContext, statement: Statement) -> Statement:
    statement.rewritten = [_rewrite_showf(ctx, stm) for stm in statement.rewritten]
    return statement
