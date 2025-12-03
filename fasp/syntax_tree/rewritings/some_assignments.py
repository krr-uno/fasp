from typing import List, Sequence

from clingo import ast, symbol
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    ChoiceSomeAssignment,
    FASP_Statement,
    HeadSimpleAssignment,
)
from fasp.util.ast import BodyLiteralAST, StatementAST, TermAST


def _transform_choice_some_to_choice_assignment[
    T: (
        ast.StatementRule,
        AssignmentRule,
    )
](library: Library, rule: T) -> T | None:
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
    if not isinstance(rule, AssignmentRule):
        return None  # unchanged

    head = rule.head
    if not isinstance(head, ChoiceSomeAssignment):
        return None  # unchanged

    body: Sequence[BodyLiteralAST] = rule.body

    # Convert BodyAggregateElements -> AssignmentAggregateElements
    new_elements: List[AssignmentAggregateElement] = []
    for elem in head.elements:
        rhs_term: TermAST = elem.tuple[0]  # first term of BodyAggregateElement

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

    # Create right guard = 1
    right_guard = ast.RightGuard(
        library,
        ast.Relation.Equal,
        ast.TermSymbolic(library, head.location, symbol.Number(library, 1)),
    )

    # Construct new ChoiceAssignment head
    new_head = ChoiceAssignment(
        location=head.location,
        elements=new_elements,
        left_guard=None,
        right_guard=right_guard,
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

    new_body: list[BodyLiteralAST] = [count_aggregate, *body]

    return rule.update(library, head=new_head, body=new_body)


def transform_choice_some_to_choice_assignment(
    library: Library, stm: FASP_Statement
) -> FASP_Statement:
    # if elif for mypy
    if isinstance(stm, ast.StatementRule):
        return _transform_choice_some_to_choice_assignment(library, stm) or stm
    elif isinstance(stm, AssignmentRule):
        return _transform_choice_some_to_choice_assignment(library, stm) or stm
    else:
        return stm
