from clingo import ast
from clingo.core import Library

from funasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from funasp.syntax_tree.collectors import collect_variables
from funasp.util.ast import (
    FreshVariableGenerator,
)


def normalize_assignment_aggregates(
    library: Library, stm: FASP_Statement
) -> FASP_Statement:
    """Rewrites rules with HeadAssignmentAggregate into rules with HeadSimpleAssignment and a BodyAggregate.
    For example, a rule like:
    ```
        f(X) := #sum { Y: p(Y,Z) } :- b(X,Z).
    ```
    would be rewritten into:
    ```
        f(X) := W :- b(X,Z); W = #sum { Y: p(Y,Z) }.
    ```
    where W is a fresh variable.
    """
    if not isinstance(stm, AssignmentRule) or not isinstance(
        head := stm.head, HeadAssignmentAggregate
    ):
        return stm
    used_variables = collect_variables(stm)
    fresh_variable_generator = FreshVariableGenerator(used_variables)
    fresh_variable = fresh_variable_generator.fresh_variable(
        library, head.location, "W"
    )
    new_head = HeadSimpleAssignment(
        head.location,
        head.assigned_function,
        fresh_variable,
    )

    # Build body aggregate W = #agg{ ... } (as a BodyAggregate with LeftGuard(W, Equal))
    body_agg = ast.BodyAggregate(
        library,
        head.location,
        ast.Sign.NoSign,
        ast.LeftGuard(library, fresh_variable, ast.Relation.Equal),
        head.aggregate_function,
        head.elements,
        None,
    )

    # Preserve the original body and append the equality-to-aggregate literal.
    new_body: list[ast.BodyLiteral] = list(stm.body) + [body_agg]

    # Return the rewritten rule.
    return stm.update(head=new_head, body=new_body)
