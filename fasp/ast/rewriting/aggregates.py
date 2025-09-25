from clingo import ast
from clingo.core import Library

from fasp.ast import (
    AssignmentRule,
    FASP_Statement,
    HeadAggregateAssignment,
    HeadSimpleAssignment,
)
from fasp.ast.rewriting.collectors import collect_variables
from fasp.util.ast import (
    BodyLiteralAST,
    FreshVariableGenerator,
)

# class HeadAggregateToBodyRewriteTransformer:
#     """
#     Rewrites head aggregates of the form:
#         f(X) = #agg{ ... } :- Body.
#     into
#         f(X) = W :- Body, W = #agg{ ... }.
#     where W is a fresh variable not occurring in the original rule.

#     Also collects syntactic errors if:
#       - comparison symbol in the head-aggregate is not '='
#       - the aggregate appears on the right-hand side (#agg{...} = f(X))
#     """

#     def __init__(self, library: Library) -> None:
#         self.library = library
#         self.errors: list[SyntacticError] = []

#     def rewrite_statements(self, statements: list[StatementAST]) -> list[StatementAST]:
#         """Return new statements, recording errors in self.errors."""
#         out: list[StatementAST] = []
#         for st in statements:
#             out.append(self._rewrite_statement(st))
#         return out

#     def _aggregate_element_head_to_body(
#         self, head_element: ast.HeadAggregateElement
#     ) -> ast.BodyAggregateElement:
#         conditions: list[LiteralAST] = []
#         if head_element.literal is not None:
#             conditions.append(head_element.literal)
#         conditions.extend(list(head_element.condition))
#         return ast.BodyAggregateElement(
#             self.library,
#             head_element.location,
#             list(head_element.tuple),
#             conditions,
#         )

#     def _rewrite_statement(self, st: StatementAST) -> StatementAST:
#         if (
#             not isinstance(st, ast.StatementRule)
#             or not st.head
#             or not isinstance(st.head, ast.HeadAggregate)
#         ):
#             return st

#         head: ast.HeadAggregate = st.head

#         # Detect whether the aggregate is on the left or right
#         left_guard: ast.LeftGuard | None = head.left
#         right_guard: ast.RightGuard | None = head.right

#         # Reject aggregates on the right like #sum{...} = f(X)"
#         if left_guard is None:
#             # Shouldn't happen for a well-formed head aggregate
#             self._error(
#                 head.location,
#                 f"Missing the left-hand side of the assignment: {str(head)}",
#                 head,
#             )
#             return st

#         if right_guard is not None:
#             self._error(
#                 head.location,
#                 f"Wrong assignment syntax: {str(head)}",
#                 head,
#             )
#             return st

#         # The comparison must be equality
#         if left_guard.relation != ast.Relation.Equal:
#             # Suggesting the correct form in error message
#             corrected_guard = left_guard.update(
#                 self.library, relation=ast.Relation.Equal
#             )
#             corrected_head = head.update(self.library, left=corrected_guard)
#             self._error(
#                 head.location,
#                 f'aggregates with comparisons cannot not be used in the head, found "{str(head)}", assignments are of the form "{str(corrected_head)}"',
#                 type(head),
#             )
#             return st

#         lhs = left_guard.term

#         if not is_function(lhs):
#             self._error(
#                 head.location,
#                 f"The left-hand side of an assignment must be a function term, "
#                 f"found {lhs}",
#                 head,
#             )
#             return st

#         # Collect used variables in this rule to generate a fresh W
#         used_variables = collect_variables([st])
#         fresh_variable_generator = FreshVariableGenerator(used_variables)
#         fresh_variable = fresh_variable_generator.fresh_variable(
#             self.library, head.location, "W"
#         )

#         # Build the new head: f(Args) = W
#         new_head_lit = ast.LiteralComparison(
#             self.library,
#             head.location,
#             ast.Sign.NoSign,
#             left_guard.term,
#             [
#                 ast.RightGuard(self.library, ast.Relation.Equal, fresh_variable),
#             ],
#         )
#         new_head = ast.HeadSimpleLiteral(self.library, new_head_lit)

#         # Convert head-aggregate elements to body-aggregate elements unchanged
#         body_elems: list[ast.BodyAggregateElement] = [
#             self._aggregate_element_head_to_body(el) for el in head.elements
#         ]

#         # Build body aggregate W = #agg{ ... } (as a BodyAggregate with LeftGuard(W, Equal))
#         body_agg = ast.BodyAggregate(
#             self.library,
#             head.location,
#             ast.Sign.NoSign,
#             ast.LeftGuard(self.library, fresh_variable, ast.Relation.Equal),
#             head.function,
#             body_elems,
#             None,
#         )

#         # Preserve the original body and append the equality-to-aggregate literal.
#         new_body: list[BodyLiteralAST] = list(st.body) + [body_agg]

#         # Return the rewritten rule.
#         return st.update(self.library, head=new_head, body=new_body)

#     def _error(
#         self, location: Location, message: str, information: Any | None = None
#     ) -> None:
#         """
#         Record a syntactic error at the given location with a structured message.
#         """
#         self.errors.append(SyntacticError(location, message, information))


def normalize_assignment_aggregates(
    library: Library, stm: FASP_Statement
) -> FASP_Statement:
    if not isinstance(stm, AssignmentRule) or not isinstance(
        head := stm.head, HeadAggregateAssignment
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
    new_body: list[BodyLiteralAST] = list(stm.body) + [body_agg]

    # Return the rewritten rule.
    return stm.update(head=new_head, body=new_body)
