"""
Normalization of aggregate assignments.

The parser desugars ``f(t) := #agg{ e1; ... } :- B.`` into the head aggregate
``Ff(t) = #agg { e1': NONE: c1; ... } :- B.`` whose left guard carries the
prefixed function (without the value slot). This module normalizes it into a
simple assignment head with a body aggregate::

    Ff(t,W) :- B; W = #agg{ e1': c1; ... }.

where ``W`` is a fresh variable, matching the old pipeline's
``normalize_assignment_aggregates`` output.

This step runs before the prefix renaming pass and after the ``#some``
rewriting, so the left-guard prefix is the parser's literal ``F``.
"""

from clingo_funasp import ast

from funasp.ast import PARSER_PREFIX
from funasp.ast._rewritings.context import RewriteContext
from funasp.util.ast import FreshVariableGenerator
from funasp.util.collectors import collect_variables


def rewrite_assignment_aggregates(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """Rewrite an aggregate assignment head into a simple head plus body aggregate."""
    library = context.lib.library
    prefix = PARSER_PREFIX
    if not isinstance(statement, ast.StatementRule) or not isinstance(
        head := statement.head, ast.HeadAggregate
    ):
        return statement
    left = head.left
    if (
        left is None
        or not isinstance(left.term, ast.TermFunction)
        or not left.term.name.startswith(prefix)
    ):
        return statement
    assert left.relation == ast.Relation.Equal
    assert head.right is None

    name = left.term.name
    assert len(left.term.pool) == 1, f"Terms must be unpooled {left.term}"
    arguments = left.term.pool[0].arguments

    used_variables = collect_variables(statement)
    fresh_variable_generator = FreshVariableGenerator(used_variables)
    fresh_variable = fresh_variable_generator.fresh_variable(
        library, head.location, "W"
    )

    atom = ast.TermFunction(
        library,
        head.location,
        name,
        [ast.ArgumentTuple(library, [*arguments, fresh_variable])],
    )
    new_head = ast.HeadSimpleLiteral(
        library,
        ast.LiteralSymbolic(library, head.location, ast.Sign.NoSign, atom),
    )

    # The parser fills the literal slot of the elements with a placeholder;
    # the body aggregate keeps only the tuple and the condition.
    body_elements = [
        ast.BodyAggregateElement(
            library, element.location, list(element.tuple), list(element.condition)
        )
        for element in head.elements
    ]
    body_aggregate = ast.BodyAggregate(
        library,
        head.location,
        ast.Sign.NoSign,
        ast.LeftGuard(library, fresh_variable, ast.Relation.Equal),
        head.function,
        body_elements,
        None,
    )
    new_body: list[ast.BodyLiteral] = [*statement.body, body_aggregate]
    return statement.update(library, head=new_head, body=new_body)
