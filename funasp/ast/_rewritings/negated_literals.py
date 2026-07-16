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

Negated aggregate *element* literals (left of the ``:`` in set-aggregate and
head-aggregate elements) over intensional functions are lifted with the same
sign-preserving encoding, for both single and double negation; function-free
negated element literals are handled natively by clingo and left untouched.
The same treatment applies to negated intensional literals in optimize
element conditions and weak-constraint bodies.

Negated *comparisons* whose intensional functions require unnesting (nested
calls, arithmetic, or non-equality relations) are lifted the same way — with
the replacement keeping the comparison's sign, single or double — in
conditions, aggregate element literals, optimize elements, and
weak-constraint bodies. A comparison binds no variables, so the auxiliary
rule additionally copies the non-negated sibling condition literals and the
enclosing rule's non-negated simple body literals as safety guards (sibling
aggregates are excluded: copying the enclosing aggregate into the auxiliary
would make it depend on its own lifted condition). Plain functional
equalities need no unnesting and are translated in place by
``prefix_comparisons``. Top-level rule-body comparisons are handled by the
``#false : l`` encoding when singly negated and by the doubly-negated body
lifting — with the rule's non-negated simple body literals as guards — when
doubly negated.

The lifting steps are driven by :class:`_NegationLifter`, which holds the
rewrite context, the clingo library, and the auxiliary rules collected for
one statement.
"""

from functools import partial
from typing import Sequence, TypeGuard

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


def _positive_simple_body_literals(
    body: Sequence[ast.BodyLiteral],
) -> list[ast.BodyLiteral]:
    """Return the non-negated simple body literals usable as outer guards.

    Sibling aggregates and conditional literals are excluded: copying the
    enclosing aggregate into an auxiliary rule would make it depend on its
    own lifted condition.
    """
    return [
        body_literal
        for body_literal in body
        if isinstance(body_literal, ast.BodySimpleLiteral)
        and body_literal.literal.sign == ast.Sign.NoSign
    ]


def _weak_body_guards(body: Sequence[ast.BodyLiteral]) -> list[ast.BodyLiteral]:
    """Return the non-negated weak-constraint body literals usable as guards."""
    return [
        body_literal
        for body_literal in body
        if (
            isinstance(body_literal, ast.BodySimpleLiteral)
            and body_literal.literal.sign == ast.Sign.NoSign
        )
        or (
            isinstance(body_literal, ast.BodyAggregate | ast.BodySetAggregate)
            and body_literal.sign == ast.Sign.NoSign
        )
    ]


class _NegationLifter:
    """Lifts the negated literals of one statement into auxiliary rules.

    Holds the rewrite context, the clingo library, and the auxiliary rules
    collected while lifting, which every step would otherwise thread by hand.
    """

    def __init__(self, context: RewriteContext):
        """Initialize the lifter for a single statement."""
        self.context = context
        self.library = context.lib.library
        self.auxiliary: list[ast.Statement] = []

    def _contains_intensional_functions(
        self, literal: ast.LiteralComparison | ast.LiteralSymbolic
    ) -> bool:
        """Whether the literal contains intensional function terms to unnest."""
        _, comparisons = unnest_functions(
            self.library,
            literal,
            self.context.intensional_functions,
            FreshVariableGenerator(),
        )
        return bool(comparisons)

    def _lift_literal(
        self,
        literal: ast.LiteralComparison | ast.LiteralSymbolic,
        aux_body: list[ast.BodyLiteral],
        *,
        replacement_sign: ast.Sign = ast.Sign.Single,
    ) -> ast.LiteralSymbolic:
        """Append the rule ``RDi(vars) :- aux_body.`` and return the replacement.

        The replacement is the ``RDi(vars)`` call under ``replacement_sign``.
        """
        location = literal.location
        variables = [
            ast.TermVariable(self.library, location, name)
            for name in sorted(collect_variables(literal))
            if name != "_"
        ]
        atom = ast.TermFunction(
            self.library,
            location,
            self.context.fresh_predicate_name(),
            [ast.ArgumentTuple(self.library, variables)],
        )
        head = ast.HeadSimpleLiteral(self.library, create_literal(self.library, atom))
        self.auxiliary.append(ast.StatementRule(self.library, location, head, aux_body))
        replacement = create_literal(self.library, atom, replacement_sign)
        assert isinstance(replacement, ast.LiteralSymbolic)
        return replacement

    def _lift_negated_literal(
        self, literal: ast.LiteralSymbolic
    ) -> ast.LiteralSymbolic:
        """Lift a negated literal, keeping its sign on the auxiliary call.

        The auxiliary rule holds the literal positively, so it binds its own
        variables and its intensional functions are unnested by the ordinary
        positive-body encoding.
        """
        positive = ast.BodySimpleLiteral(
            self.library, literal.update(self.library, sign=ast.Sign.NoSign)
        )
        return self._lift_literal(literal, [positive], replacement_sign=literal.sign)

    def _lift_negated_intensional_comparison(
        self, guards: list[ast.BodyLiteral], literal: ast.Literal
    ) -> ast.LiteralSymbolic | None:
        """Lift a negated comparison over intensional functions, keeping its sign.

        Covers single and double negation; the replacement carries the
        comparison's sign. A comparison binds no variables, so the auxiliary
        rule copies the sibling non-negated literals as guards before the
        positive comparison, whose intensional functions are then unnested by
        the ordinary positive-body encoding. Plain functional equalities need
        no unnesting and are left to ``prefix_comparisons``, as are
        function-free comparisons.
        """
        if (
            not isinstance(literal, ast.LiteralComparison)
            or literal.sign == ast.Sign.NoSign
            or not self._contains_intensional_functions(literal)
        ):
            return None
        positive = ast.BodySimpleLiteral(
            self.library, literal.update(self.library, sign=ast.Sign.NoSign)
        )
        return self._lift_literal(
            literal, [*guards, positive], replacement_sign=literal.sign
        )

    def _condition_guards(
        self, condition: Sequence[ast.Literal]
    ) -> list[ast.BodyLiteral]:
        """Wrap the non-negated literals of a condition as auxiliary-rule guards."""
        return [
            ast.BodySimpleLiteral(self.library, literal)
            for literal in condition
            if literal.sign == ast.Sign.NoSign
        ]

    def _lift_condition_literal(
        self, guards: list[ast.BodyLiteral], literal: ast.Literal
    ) -> ast.LiteralSymbolic | None:
        """Replace a negated condition literal by a fresh auxiliary call.

        Symbolic single negations are always lifted; symbolic double
        negations and negated comparisons only when they contain intensional
        functions.
        """
        replacement = self._lift_negated_intensional_comparison(guards, literal)
        if replacement is not None:
            return replacement
        if not isinstance(literal, ast.LiteralSymbolic):
            return None
        if literal.sign == ast.Sign.Double:
            if not self._contains_intensional_functions(literal):
                return None
        elif literal.sign != ast.Sign.Single:
            return None
        return self._lift_negated_literal(literal)

    def _rewrite_element_condition[
        ElementT: (
            ast.BodyConditionalLiteral,
            ast.HeadConditionalLiteral,
            ast.SetAggregateElement,
            ast.HeadAggregateElement,
            ast.BodyAggregateElement,
        )
    ](
        self, outer_guards: list[ast.BodyLiteral], element: ElementT
    ) -> ElementT | None:
        """Lift the negated literals inside an element's condition."""
        guards = [*outer_guards, *self._condition_guards(element.condition)]
        new_condition = map_none(
            partial(self._lift_condition_literal, guards), element.condition
        )
        if new_condition is None:
            return None
        return element.update(self.library, condition=new_condition)

    def _lift_negated_intensional_literal(
        self, literal: ast.Literal
    ) -> ast.LiteralSymbolic | None:
        """Lift a negated symbolic literal when it contains intensional functions.

        Function-free negated literals are handled natively by clingo and
        left untouched.
        """
        if (
            not isinstance(literal, ast.LiteralSymbolic)
            or literal.sign == ast.Sign.NoSign
            or not self._contains_intensional_functions(literal)
        ):
            return None
        return self._lift_negated_literal(literal)

    def _rewrite_aggregate_element[
        ElementT: (ast.SetAggregateElement, ast.HeadAggregateElement)
    ](
        self, outer_guards: list[ast.BodyLiteral], element: ElementT
    ) -> ElementT | None:
        """Lift an element's negated intensional literal and condition literals."""
        update: dict[str, object] = {}
        guards = [*outer_guards, *self._condition_guards(element.condition)]
        replacement = self._lift_negated_intensional_comparison(guards, element.literal)
        if replacement is None:
            replacement = self._lift_negated_intensional_literal(element.literal)
        if replacement is not None:
            update["literal"] = replacement
        new_condition = map_none(
            partial(self._lift_condition_literal, guards), element.condition
        )
        if new_condition is not None:
            update["condition"] = new_condition
        if not update:
            return None
        return element.update(self.library, **update)

    def _lift_optimize_condition_literal(
        self, guards: list[ast.BodyLiteral], literal: ast.Literal
    ) -> ast.LiteralSymbolic | None:
        """Lift a negated intensional literal or comparison in an optimize condition."""
        replacement = self._lift_negated_intensional_comparison(guards, literal)
        if replacement is not None:
            return replacement
        return self._lift_negated_intensional_literal(literal)

    def _rewrite_optimize_element(
        self, element: ast.OptimizeElement
    ) -> ast.OptimizeElement | None:
        """Lift the negated intensional literals of an optimize element condition."""
        guards = self._condition_guards(element.condition)
        new_condition = map_none(
            partial(self._lift_optimize_condition_literal, guards), element.condition
        )
        if new_condition is None:
            return None
        return element.update(self.library, condition=new_condition)

    def _rewrite_weak_body_literal(
        self, guards: list[ast.BodyLiteral], body_literal: ast.BodyLiteral
    ) -> ast.BodyLiteral | None:
        """Lift the negated intensional literals of a weak-constraint body."""
        if isinstance(body_literal, ast.BodySimpleLiteral):
            replacement = self._lift_negated_intensional_comparison(
                guards, body_literal.literal
            )
            if replacement is None:
                replacement = self._lift_negated_intensional_literal(
                    body_literal.literal
                )
            if replacement is None:
                return None
            return ast.BodySimpleLiteral(self.library, replacement)
        # Nested conditions must not receive sibling aggregates as guards: the
        # enclosing aggregate would end up guarding its own lifted condition.
        simple_guards: list[ast.BodyLiteral] = [
            guard for guard in guards if isinstance(guard, ast.BodySimpleLiteral)
        ]
        return self._rewrite_body_element(simple_guards, body_literal)

    def _rewrite_body_element(
        self, outer_guards: list[ast.BodyLiteral], body_literal: ast.BodyLiteral
    ) -> ast.BodyLiteral | None:
        """Lift the negated condition literals inside a single body literal."""
        if isinstance(body_literal, ast.BodyConditionalLiteral):
            return self._rewrite_element_condition(outer_guards, body_literal)
        if isinstance(body_literal, ast.BodyAggregate | ast.BodySetAggregate):
            # The two aggregate forms differ only in their element rewriter:
            # body-aggregate elements have no element literal to lift.
            if isinstance(body_literal, ast.BodyAggregate):
                new_elements = map_none(
                    partial(self._rewrite_element_condition, outer_guards),
                    body_literal.elements,
                )
            else:
                new_elements = map_none(
                    partial(self._rewrite_aggregate_element, outer_guards),
                    body_literal.elements,
                )
            if new_elements is None:
                return None
            return body_literal.update(self.library, elements=new_elements)
        return None

    def _rewrite_disjunction_element(
        self, outer_guards: list[ast.BodyLiteral], element: ast.DisjunctionElement
    ) -> ast.HeadConditionalLiteral | None:
        """Lift the negated condition literals of a conditional disjunct."""
        if not isinstance(element, ast.HeadConditionalLiteral):
            return None
        return self._rewrite_element_condition(outer_guards, element)

    def _rewrite_head(
        self, outer_guards: list[ast.BodyLiteral], head: ast.HeadLiteral
    ) -> ast.HeadLiteral | None:
        """Lift the negated condition literals inside a rule head."""
        if isinstance(head, ast.HeadDisjunction):
            new_elements = map_none(
                partial(self._rewrite_disjunction_element, outer_guards),
                head.elements,
            )
        elif isinstance(head, ast.HeadSetAggregate | ast.HeadAggregate):
            new_elements = map_none(
                partial(self._rewrite_aggregate_element, outer_guards),
                head.elements,
            )
        else:
            return None
        if new_elements is None:
            return None
        return head.update(self.library, elements=new_elements)

    def rewrite_statement(self, statement: ast.Statement) -> list[ast.Statement]:
        """Lift the negated condition literals of one statement.

        Returns the rewritten statement followed by the collected auxiliary
        rules; statements without lifted literals pass through unchanged.
        """
        if isinstance(statement, ast.StatementOptimize):
            new_elements = map_none(self._rewrite_optimize_element, statement.elements)
            if new_elements is None:
                return [statement]
            return [
                statement.update(self.library, elements=new_elements),
                *self.auxiliary,
            ]
        if isinstance(statement, ast.StatementWeakConstraint):
            weak_guards = _weak_body_guards(statement.body)
            new_weak_body = map_none(
                partial(self._rewrite_weak_body_literal, weak_guards), statement.body
            )
            if new_weak_body is None:
                return [statement]
            return [
                statement.update(self.library, body=new_weak_body),
                *self.auxiliary,
            ]
        if not isinstance(statement, ast.StatementRule):
            return [statement]
        update: dict[str, object] = {}
        rule_guards = _positive_simple_body_literals(statement.body)
        new_head = self._rewrite_head(rule_guards, statement.head)
        if new_head is not None:
            update["head"] = new_head
        new_body = map_none(
            partial(self._rewrite_body_element, rule_guards), statement.body
        )
        if new_body is not None:
            update["body"] = new_body
        if not update:
            return [statement]
        return [statement.update(self.library, **update), *self.auxiliary]

    def _lift_double_negated_body_literal(
        self, guards: list[ast.BodyLiteral], body_literal: ast.BodyLiteral
    ) -> ast.BodySimpleLiteral | None:
        """Lift a doubly negated intensional body literal, if it is one.

        Doubly negated comparisons are lifted with the sibling guards, like
        their condition counterparts; symbolic literals bind their own
        variables and need none.
        """
        if (
            not isinstance(body_literal, ast.BodySimpleLiteral)
            or body_literal.literal.sign != ast.Sign.Double
        ):
            return None
        replacement = self._lift_negated_intensional_comparison(
            guards, body_literal.literal
        )
        if replacement is None:
            if not isinstance(
                body_literal.literal, ast.LiteralSymbolic
            ) or not self._contains_intensional_functions(body_literal.literal):
                return None
            replacement = self._lift_negated_literal(body_literal.literal)
        return ast.BodySimpleLiteral(self.library, replacement)

    def rewrite_double_negated_body(
        self, statement: ast.Statement
    ) -> list[ast.Statement]:
        """Lift the doubly negated intensional body literals of a rule."""
        if not isinstance(statement, ast.StatementRule):
            return [statement]
        guards = _positive_simple_body_literals(statement.body)
        new_body = map_none(
            partial(self._lift_double_negated_body_literal, guards), statement.body
        )
        if new_body is None:
            return [statement]
        return [statement.update(self.library, body=new_body), *self.auxiliary]


def rewrite_negated_condition_literals(
    context: RewriteContext, statement: ast.Statement
) -> list[ast.Statement]:
    """
    Lift negated condition literals of a statement into auxiliary rules.

    Covers rules, optimize statements (element conditions), and weak
    constraints (body literals). Returns the rewritten statement followed by
    the auxiliary rules defining the fresh predicates that replace the lifted
    literals.
    """
    return _NegationLifter(context).rewrite_statement(statement)


def rewrite_double_negated_body_literals(
    context: RewriteContext, statement: ast.Statement
) -> list[ast.Statement]:
    """
    Lift doubly negated body literals over intensional functions.

    Each such ``not not l`` is replaced by ``not not RDi(vars)``, keeping the
    double negation, and defined by the auxiliary rule ``RDi(vars) :- l.``
    with ``l`` positive — the same encoding as the condition-literal lifting.
    Doubly negated comparisons requiring unnesting are lifted too; as they
    bind no variables, the auxiliary rule copies the rule's non-negated
    simple body literals as safety guards. Returns the rewritten statement
    followed by the auxiliary rules; statements without such literals pass
    through unchanged.
    """
    return _NegationLifter(context).rewrite_double_negated_body(statement)


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
