from typing import Iterable

from clingo.ast import StatementShowSignature

from fasp.syntax_tree._context import RewriteContext
from fasp.syntax_tree._nodes import FASP_Statement, ShowFDirective


def showf_to_show_transformer(
    ctx: RewriteContext, statements: Iterable[FASP_Statement]
) -> Iterable[FASP_Statement]:
    library = ctx.elib.library
    prefix = ctx.prefix
    out = []
    for stmt in statements:
        if isinstance(stmt, ShowFDirective):
            sig = getattr(stmt, "signature", None)
            if sig is not None:
                # Transform f/2 → Ff/3
                name = prefix + sig.name
                arity = sig.arity + 1
                out.append(
                    StatementShowSignature(
                        library,
                        stmt.location,
                        name,
                        arity,
                        sign=False,
                        value=True,
                    )
                )
            # else:
            #     out.append(
            #         StatementShow(
            #             library,
            #             stmt.location,
            #             TermFunction(library, stmt.location, prefix + "NONE", []),
            #             [],
            #         )
            #     )
        else:
            out.append(stmt)
    return out
