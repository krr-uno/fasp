"""
Rewriting of ``#some`` assignments.

The parser desugars ``f(t) := #some{ v1: c1; ... } :- B.`` into the head
aggregate ``FSf(t) = #sum { v1: NONE: c1; ... } :- B.`` where the ``FS``
prefix marks the ``#some``. This module rewrites it into a choice with the
"someness" semantics::

    { Ff(t,v1): c1; ... } = 1 :- #count{ v1: c1; ... } >= 1, B.

i.e. exactly one value is chosen whenever at least one candidate exists.
The ``FS`` marker is unambiguous: user-written function names cannot start
with an uppercase letter, so ``F`` + name never yields ``FS...``.

This step runs before the prefix renaming pass, so the marker is always the
parser's literal ``FS``.
"""

from typing import Sequence

from clingo_funasp import ast, symbol

from funasp.ast import PARSER_PREFIX, SOME_MARKER
from funasp.rewriting._context import RewriteContext

#: The prefix the parser generates for ``#some`` assignments.
SOME_PREFIX = PARSER_PREFIX + SOME_MARKER


def rewrite_some_assignments(
    context: RewriteContext, statement: ast.Statement
) -> list[ast.Statement]:
    """Rewrite a ``#some`` assignment head into choices with a guard body.

    Returns one statement per pool entry on the left guard, so an unpooled
    term yields a single statement and ``f(a;b) := #some{...}`` yields one
    statement for ``f(a)`` and one for ``f(b)``. Non-``#some`` statements pass
    through unchanged as a one-element list.
    """

    if not isinstance(statement, ast.StatementRule) or not isinstance(
        head := statement.head, ast.HeadAggregate
    ):
        return [statement]
    left = head.left
    if (
        left is None
        or not isinstance(left.term, ast.TermFunction)
        or not left.term.name.startswith(SOME_PREFIX)
    ):
        return [statement]
    assert left.relation == ast.Relation.Equal
    assert head.right is None

    name = PARSER_PREFIX + left.term.name[len(SOME_PREFIX) :]
    return [
        _rewrite_some_pool_entry(context, statement, head, name, entry.arguments)
        for entry in left.term.pool
    ]


def _rewrite_some_pool_entry(
    context: RewriteContext,
    statement: ast.StatementRule,
    head: ast.HeadAggregate,
    name: str,
    arguments: Sequence[ast.Term | ast.Projection],
) -> ast.Statement:
    """Build the choice statement for a single pool entry of a ``#some`` head."""

    library = context.lib.library

    choice_elements: list[ast.SetAggregateElement] = []
    count_elements: list[ast.BodyAggregateElement] = []
    for element in head.elements:
        if len(element.tuple) == 1:
            value: ast.Term = element.tuple[0]
        else:
            value = ast.TermTuple(
                library,
                head.location,
                [ast.ArgumentTuple(library, list(element.tuple))],
            )
        atom = ast.TermFunction(
            library,
            head.location,
            name,
            [ast.ArgumentTuple(library, [*arguments, value])],
        )
        literal = ast.LiteralSymbolic(library, head.location, ast.Sign.NoSign, atom)
        choice_elements.append(
            ast.SetAggregateElement(
                library, element.location, literal, list(element.condition)
            )
        )
        count_elements.append(
            ast.BodyAggregateElement(
                library, element.location, list(element.tuple), list(element.condition)
            )
        )

    new_head = ast.HeadSetAggregate(
        library,
        head.location,
        None,
        choice_elements,
        ast.RightGuard(
            library,
            ast.Relation.Equal,
            ast.TermSymbolic(library, head.location, symbol.Number(library, 1)),
        ),
    )
    count_aggregate = ast.BodyAggregate(
        library,
        head.location,
        ast.Sign.NoSign,
        None,
        ast.AggregateFunction.Count,
        count_elements,
        ast.RightGuard(
            library,
            ast.Relation.GreaterEqual,
            ast.TermSymbolic(library, head.location, symbol.Number(library, 1)),
        ),
    )
    new_body: list[ast.BodyLiteral] = [count_aggregate, *statement.body]
    return statement.update(library, head=new_head, body=new_body)
