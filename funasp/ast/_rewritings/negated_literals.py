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

Doubly negated literals over intensional functions — top-level body literals
as well as condition literals — are lifted the same way, keeping the double
negation on the auxiliary call: ``not not l`` becomes ``not not RDi(v1,...,vm)``
defined by the auxiliary rule ``RDi(v1,...,vm) :- l.`` where ``l`` appears
positively (binding its own variables) and is unnested by the ordinary
positive-body encoding. The rule head's dependency on the function stays
non-positive because it passes through the double negation. Doubly negated
literals without intensional functions are left untouched.
"""

from functools import partial
from typing import TypeGuard

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.ast._rewritings.context import RewriteContext
from funasp.ast._rewritings.literals import unnest_functions
from funasp.util.ast import FreshVariableGenerator, create_literal
from funasp.util.collectors import collect_variables
from funasp.util.iterables import map_none

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
    new_body = map_none(
        partial(_rewrite_body_literal, context.lib.library), statement.body
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


def _contains_intensional_functions(
    context: RewriteContext, literal: ast.LiteralSymbolic
) -> bool:
    """Whether the literal contains intensional function terms."""
    _, comparisons = unnest_functions(
        context.lib.library,
        literal,
        context.intensional_functions,
        FreshVariableGenerator(),
    )
    return bool(comparisons)


def _lift_literal(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    literal: ast.LiteralSymbolic,
    aux_body: list[ast.BodyLiteral],
    replacement_sign: ast.Sign = ast.Sign.Single,
) -> ast.LiteralSymbolic:
    """Append the rule ``RDi(vars) :- aux_body.`` and return the replacement.

    The replacement is the ``RDi(vars)`` call under ``replacement_sign``.
    """
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
    auxiliary.append(ast.StatementRule(library, location, head, aux_body))
    replacement = create_literal(library, atom, replacement_sign)
    assert isinstance(replacement, ast.LiteralSymbolic)
    return replacement


def _lift_negated_literal(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    literal: ast.LiteralSymbolic,
) -> ast.LiteralSymbolic:
    """Lift a negated literal, keeping its sign on the auxiliary call.

    The auxiliary rule holds the literal positively, so it binds its own
    variables and its intensional functions are unnested by the ordinary
    positive-body encoding.
    """
    positive = ast.BodySimpleLiteral(
        library, literal.update(library, sign=ast.Sign.NoSign)
    )
    return _lift_literal(
        context, auxiliary, library, literal, [positive], replacement_sign=literal.sign
    )


def _lift_condition_literal(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    literal: ast.Literal,
) -> ast.LiteralSymbolic | None:
    """Replace a negated symbolic condition literal by a fresh auxiliary call.

    Single negations are always lifted; double negations only when they
    contain intensional functions.
    """
    if not isinstance(literal, ast.LiteralSymbolic):
        return None
    if literal.sign == ast.Sign.Double:
        if not _contains_intensional_functions(context, literal):
            return None
    elif literal.sign != ast.Sign.Single:
        return None
    return _lift_negated_literal(context, auxiliary, library, literal)


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
    new_condition = map_none(
        partial(_lift_condition_literal, context, auxiliary, library),
        element.condition,
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
        new_elements = map_none(
            partial(_rewrite_element_condition, context, auxiliary, library),
            body_literal.elements,
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
        new_elements = map_none(
            partial(_rewrite_disjunction_element, context, auxiliary, library),
            head.elements,
        )
    elif isinstance(head, ast.HeadSetAggregate | ast.HeadAggregate):
        new_elements = map_none(
            partial(_rewrite_element_condition, context, auxiliary, library),
            head.elements,
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
    new_body = map_none(
        partial(_rewrite_body_element, context, auxiliary, library),
        statement.body,
    )
    if new_body is not None:
        update["body"] = new_body
    if not update:
        return [statement]
    return [statement.update(library, **update), *auxiliary]


def _lift_double_negated_body_literal(
    context: RewriteContext,
    auxiliary: list[ast.Statement],
    library: Library,
    body_literal: ast.BodyLiteral,
) -> ast.BodySimpleLiteral | None:
    """Lift a doubly negated intensional body literal, if it is one."""
    if (
        not isinstance(body_literal, ast.BodySimpleLiteral)
        or not isinstance(body_literal.literal, ast.LiteralSymbolic)
        or body_literal.literal.sign != ast.Sign.Double
        or not _contains_intensional_functions(context, body_literal.literal)
    ):
        return None
    replacement = _lift_negated_literal(
        context, auxiliary, library, body_literal.literal
    )
    return ast.BodySimpleLiteral(library, replacement)


def rewrite_double_negated_body_literals(
    context: RewriteContext, statement: ast.Statement
) -> list[ast.Statement]:
    """
    Lift doubly negated body literals over intensional functions.

    Each such ``not not l`` is replaced by ``not not RDi(vars)``, keeping the
    double negation, and defined by the auxiliary rule ``RDi(vars) :- l.``
    with ``l`` positive — the same encoding as the condition-literal lifting.
    Returns the rewritten statement followed by the auxiliary rules;
    statements without such literals pass through unchanged.
    """
    if not isinstance(statement, ast.StatementRule):
        return [statement]
    library = context.lib.library
    auxiliary: list[ast.Statement] = []
    new_body = map_none(
        partial(_lift_double_negated_body_literal, context, auxiliary, library),
        statement.body,
    )
    if new_body is None:
        return [statement]
    return [statement.update(library, body=new_body), *auxiliary]


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
