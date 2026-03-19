from typing import List

from clingo import ast, symbol

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import (
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    ChoiceSomeAssignment,
    FASP_Statement,
    HeadSimpleAssignment,
)
from funasp.syntax_tree.rewritings.unnesting._statement import Statement


def _rewrite_some_choices(
    context: RewriteContext, stm: FASP_Statement
) -> FASP_Statement:
    """
    Transform a ChoiceSomeAssignment head into a ChoiceAssignment head
    with a corresponding #count aggregate prepended to the body.

    Input:
        A Rule (AssignmentRule) with ChoiceSomeAssignment as Head.
        a := #some{X : p(X)} :- Body.
    Output:
        An AssignmentRule with ChoiceAssignment as head.
        {a := X : p(X)} = 1 :- #count{X : p(X)} >= 1, Body.

    Returns None if no transformation is done.
    """
    library = context.lib.library
    if not isinstance(stm, AssignmentRule) or not isinstance(
        head := stm.head, ChoiceSomeAssignment
    ):
        return stm

    body = stm.body

    # Convert BodyAggregateElements -> AssignmentAggregateElements
    new_elements: List[AssignmentAggregateElement] = []
    for elem in head.elements:
        if len(elem.tuple) == 1:
            rhs_term = elem.tuple[0]
        else:
            rhs_term = ast.TermTuple(
                library, head.location, [ast.ArgumentTuple(library, elem.tuple)]
            )

        assignment = HeadSimpleAssignment(
            location=head.location,
            assigned_function=head.assigned_function,
            value=rhs_term,
        )

        new_elements.append(
            AssignmentAggregateElement(
                location=head.location,
                assignment=assignment,
                condition=list(elem.condition),
            )
        )

    # Construct new ChoiceAssignment head
    new_head = ChoiceAssignment(
        location=head.location,
        elements=new_elements,
        left=None,
        right=ast.RightGuard(
            library,
            ast.Relation.Equal,
            ast.TermSymbolic(library, head.location, symbol.Number(library, 1)),
        ),
    )

    # Construct #count aggregate for the body
    count_aggregate = ast.BodyAggregate(
        lib=library,
        location=head.location,
        sign=ast.Sign.NoSign,
        left=None,
        function=ast.AggregateFunction.Count,
        elements=head.elements,
        right=ast.RightGuard(
            library,
            ast.Relation.GreaterEqual,
            ast.TermSymbolic(library, head.location, symbol.Number(library, 1)),
        ),
    )

    new_body: list[ast.BodyLiteral] = [count_aggregate, *body]

    return stm.update(library, head=new_head, body=new_body)


def rewrite_some_choices(ctx: RewriteContext, statement: Statement) -> Statement:
    statement.rewritten = [
        _rewrite_some_choices(ctx, stm) for stm in statement.rewritten
    ]
    return statement
