"""
Rewriting of negated body literals.

Single-negation body literals ``not l`` are rewritten into the conditional
literal ``#false : l`` so that later unnesting of intensional functions inside
them can use anonymous projections.

Negated head literals are moved to the body with their sign complemented, so
``a, not b, not not c :- d.`` becomes ``a :- d, not not b, not c.``.

Negated literals inside conditions cannot become nested conditional literals,
so each ``not p(t1,...,tk)`` in a condition is replaced by
``not RDi(v1,...,vm)`` (the distinct variables of the literal) and defined by
an auxiliary rule ``RDi(v1,...,vm) :- p(t1,...,tk).`` where the literal
appears positively. This applies to the conditions of conditional literals
(body and head disjunction elements) and of aggregate and set-aggregate
elements (head and body).
"""

from functools import partial
from typing import TypeGuard

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.ast import transform_iterable
from funasp.ast._rewritings.context import RewriteContext
from funasp.util.ast import create_literal
from funasp.util.collectors import collect_variables

# A head literal is moved to the body with its negation complemented:
# ``not l`` (single) becomes ``not not l`` (double) and vice versa.
_COMPLEMENTED_SIGN = {
    ast.Sign.Single: ast.Sign.Double,
    ast.Sign.Double: ast.Sign.Single,
}


def _rewrite_body_literal(
    library: Library, literal: ast.BodyLiteral
) -> None | ast.BodyConditionalLiteral:
    """Rewrite a negated body literal into an equivalent conditional literal when needed."""
    if (
        not isinstance(literal, ast.BodySimpleLiteral)
        or isinstance(literal.literal, ast.LiteralBoolean)
        or literal.literal.sign != ast.Sign.Single
    ):
        return None

    lit = literal.literal

    # Build #false
    false_lit = ast.LiteralBoolean(library, lit.location, ast.Sign.NoSign, False)

    # Create conditional literal: #false : r(X)
    return ast.BodyConditionalLiteral(
        library, lit.location, false_lit, [lit.update(library, sign=ast.Sign.NoSign)]
    )


def rewrite_negated_body_literals(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """Rewrite eligible negated body literals inside a single statement."""
    if not isinstance(statement, ast.StatementRule):
        return statement
    new_body = transform_iterable(
        context.lib.library, statement.body, _rewrite_body_literal
    )
    if new_body is None:
        return statement
    return statement.update(context.lib.library, body=new_body)


def _is_negated_literal(node: object) -> TypeGuard[ast.LiteralSymbolic]:
    """Whether ``node`` is a symbolic literal under single or double negation."""
    return isinstance(node, ast.LiteralSymbolic) and node.sign in _COMPLEMENTED_SIGN


def _complement_to_body(
    library: Library, literal: ast.LiteralSymbolic
) -> ast.BodySimpleLiteral:
    """Turn a negated head literal into a body literal with complemented sign."""
    complemented = literal.update(library, sign=_COMPLEMENTED_SIGN[literal.sign])
    return ast.BodySimpleLiteral(library, complemented)


def _lift_condition_literal(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    literal: ast.Literal,
) -> ast.LiteralSymbolic | None:
    """Replace a negated symbolic condition literal by a fresh auxiliary call."""
    if not isinstance(literal, ast.LiteralSymbolic) or literal.sign != ast.Sign.Single:
        return None
    location = literal.location
    variables = [
        ast.TermVariable(library, location, name)
        for name in sorted(collect_variables(literal))
        if name != "_"
    ]
    atom = ast.TermFunction(
        library,
        location,
        context.fresh_predicate_name(),
        [ast.ArgumentTuple(library, variables)],
    )
    head = ast.HeadSimpleLiteral(library, create_literal(library, atom))
    positive = ast.BodySimpleLiteral(
        library, literal.update(library, sign=ast.Sign.NoSign)
    )
    auxiliary.append(ast.StatementRule(library, location, head, [positive]))
    replacement = create_literal(library, atom, ast.Sign.Single)
    assert isinstance(replacement, ast.LiteralSymbolic)
    return replacement


def _rewrite_element_condition[
    ElementT: (
        ast.BodyConditionalLiteral,
        ast.HeadConditionalLiteral,
        ast.SetAggregateElement,
        ast.HeadAggregateElement,
        ast.BodyAggregateElement,
    )
](
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    element: ElementT,
) -> ElementT | None:
    """Lift the negated literals inside an element's condition."""
    new_condition = transform_iterable(
        library,
        element.condition,
        partial(_lift_condition_literal, context, auxiliary),
    )
    if new_condition is None:
        return None
    return element.update(library, condition=new_condition)


def _rewrite_body_element(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    body_literal: ast.BodyLiteral,
) -> ast.BodyLiteral | None:
    """Lift the negated condition literals inside a single body literal."""
    if isinstance(body_literal, ast.BodyConditionalLiteral):
        return _rewrite_element_condition(context, auxiliary, library, body_literal)
    if isinstance(body_literal, ast.BodyAggregate | ast.BodySetAggregate):
        new_elements = transform_iterable(
            library,
            body_literal.elements,
            partial(_rewrite_element_condition, context, auxiliary),
        )
        if new_elements is None:
            return None
        return body_literal.update(library, elements=new_elements)
    return None


def _rewrite_disjunction_element(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    element: ast.DisjunctionElement,
) -> ast.HeadConditionalLiteral | None:
    """Lift the negated condition literals of a conditional disjunct."""
    if not isinstance(element, ast.HeadConditionalLiteral):
        return None
    return _rewrite_element_condition(context, auxiliary, library, element)


def _rewrite_head(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    head: ast.HeadLiteral,
) -> ast.HeadLiteral | None:
    """Lift the negated condition literals inside a rule head."""
    if isinstance(head, ast.HeadDisjunction):
        new_elements = transform_iterable(
            library,
            head.elements,
            partial(_rewrite_disjunction_element, context, auxiliary),
        )
    elif isinstance(head, ast.HeadSetAggregate | ast.HeadAggregate):
        new_elements = transform_iterable(
            library,
            head.elements,
            partial(_rewrite_element_condition, context, auxiliary),
        )
    else:
        return None
    if new_elements is None:
        return None
    return head.update(library, elements=new_elements)


def rewrite_negated_condition_literals(
    context: RewriteContext, statement: ast.Statement
) -> list[ast.Statement]:
    """
    Lift negated condition literals of a statement into auxiliary rules.

    Returns the rewritten statement followed by the auxiliary rules defining
    the fresh predicates that replace the lifted literals.
    """
    if not isinstance(statement, ast.StatementRule):
        return [statement]
    library = context.lib.library
    auxiliary: list[ast.Statement] = []
    update: dict[str, object] = {}
    new_head = _rewrite_head(context, auxiliary, library, statement.head)
    if new_head is not None:
        update["head"] = new_head
    new_body = transform_iterable(
        library,
        statement.body,
        partial(_rewrite_body_element, context, auxiliary),
    )
    if new_body is not None:
        update["body"] = new_body
    if not update:
        return [statement]
    return [statement.update(library, **update), *auxiliary]


def rewrite_negated_head_literals(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """Move negated literals from the head to the body with complemented sign."""
    if not isinstance(statement, ast.StatementRule):
        return statement
    head = statement.head
    library = context.lib.library
    if isinstance(head, ast.HeadSimpleLiteral):
        if not _is_negated_literal(head.literal):
            return statement
        constraint_head = ast.HeadDisjunction(library, head.literal.location, [])
        body = list(statement.body) + [_complement_to_body(library, head.literal)]
        return statement.update(library, head=constraint_head, body=body)
    if not isinstance(head, ast.HeadDisjunction):
        return statement
    kept: list[ast.DisjunctionElement] = []
    moved: list[ast.BodySimpleLiteral] = []
    for element in head.elements:
        if _is_negated_literal(element):
            moved.append(_complement_to_body(library, element))
        else:
            kept.append(element)
    if not moved:
        return statement
    new_head = head.update(library, elements=kept)
    return statement.update(library, head=new_head, body=list(statement.body) + moved)
