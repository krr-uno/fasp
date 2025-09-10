from clingo import ast
from clingo.core import Library, Location
from clingo.symbol import SymbolType

from fasp.ast.syntax_checking import (
    SyntacticError,
)
from fasp.util.ast import (
    BodyLiteralAST,
    FreshVariableGenerator,
    LiteralAST,
    StatementAST,
    collect_variables,
)


class HeadAggregateToBodyRewriteTransformer:
    """
    Rewrites head aggregates of the form:
        f(X) = #agg{ ... } :- Body.
    into
        f(X) = W :- Body, W = #agg{ ... }.
    where W is a fresh variable not occurring in the original rule.

    Also collects syntactic errors if:
      - comparison symbol in the head-aggregate is not '='
      - the aggregate appears on the right-hand side (#agg{...} = f(X))
    """

    _SUPPORTED_FUNS = {
        ast.AggregateFunction.Sum,
        ast.AggregateFunction.Count,
        ast.AggregateFunction.Max,
        ast.AggregateFunction.Min,
    }

    def __init__(self, library: Library) -> None:
        self.library = library
        self.errors: list[SyntacticError] = []

    def rewrite_statements(self, statements: list[StatementAST]) -> list[StatementAST]:
        """Return new statements, recording errors in self.errors."""
        out: list[StatementAST] = []
        for st in statements:
            out.append(self._rewrite_statement(st))
        return out

    def _rewrite_statement(self, st: StatementAST) -> StatementAST:
        if not isinstance(st, ast.StatementRule):
            return st

        head = st.head
        if not isinstance(head, ast.HeadAggregate):
            return st

        # Detect whether the aggregate is on the left or right
        left_guard = head.left  # ast.LeftGuard | None
        right_guard = head.right  # ast.RightGuard | None

        # Reject aggregates on the right like #sum{...} = f(X)"
        if right_guard is not None:
            self._error(
                head.location,
                "Head aggregate cannot appear on the right-hand side of the assignment",
                type(head),
            )
            return st

        if left_guard is None:
            # Shouldn't happen for a well-formed head aggregate
            self._error(
                head.location,
                "Head aggregate is missing left guard of the assignment",
                type(head),
            )
            return st

        # The comparison must be equality
        if left_guard.relation != ast.Relation.Equal:
            # Suggesting the correct form in error message
            corrected_guard = ast.LeftGuard(
                self.library, left_guard.term, ast.Relation.Equal
            )
            corrected_head = ast.HeadAggregate(
                self.library,
                head.location,
                corrected_guard,
                head.function,
                head.elements,
                right_guard,
            )
            self._error(
                head.location,
                f'aggregates with comparisons cannot not be used in the head, found "{str(head)}", assignments are of the form "{str(corrected_head)}"',
                type(head),
            )
            return st

        lhs = left_guard.term
        # QUERY: Should all the types [except(TermSymbolic with SymbolType Number and String)] under util.ast.TermAST be allowed?
        if isinstance(lhs, ast.TermFunction):
            pass
        elif isinstance(lhs, ast.TermSymbolic):
            if lhs.symbol.type == SymbolType.Function:
                pass
            else:
                self._error(
                    head.location,
                    f"The left-hand side of an assignment must be a function term, "
                    f"found {type(lhs)} with symbol {lhs.symbol.type}",
                    type(head),
                )
                return st
        else:
            self._error(
                head.location,
                f"The left-hand side of an assignment must be a function term, "
                f"found {type(lhs)}",
                type(head),
            )
            return st

        # if isinstance(lhs, ast.TermSymbolic):
        #     if lhs.symbol.type in (SymbolType.Number, SymbolType.String):
        #         self._error(
        #             head.location,
        #             f"The left-hand side of an assignment must be a function term, "
        #             f"found {type(lhs)} with symbol {lhs.symbol.type}",
        #             type(head),
        #         )
        #         return st

        # Collect used variables in this rule to generate a fresh W
        used = collect_variables([st])
        gen = FreshVariableGenerator(used)
        W = gen.fresh_variable(self.library, head.location, "W")

        # Build the new head: f(Args) = W
        f_term = left_guard.term  # ast.TermFunction
        new_head_lit = ast.LiteralComparison(
            self.library,
            head.location,
            ast.Sign.NoSign,
            f_term,
            [
                ast.RightGuard(self.library, ast.Relation.Equal, W),
            ],
        )
        new_head = ast.HeadSimpleLiteral(self.library, new_head_lit)

        # Convert head-aggregate elements to body-aggregate elements unchanged
        body_elems: list[ast.BodyAggregateElement] = []

        for el in head.elements:
            conditions: list[LiteralAST] = []
            if el.literal is not None:
                conditions.append(el.literal)
            conditions.extend(list(el.condition))
            # el: ast.HeadAggregateElement

            body_elems.append(
                ast.BodyAggregateElement(
                    self.library,
                    el.location,
                    list(el.tuple),
                    conditions,
                )
            )

        # Build body aggregate W = #agg{ ... } (as a BodyAggregate with LeftGuard(W, Equal))
        body_agg = ast.BodyAggregate(
            self.library,
            head.location,
            ast.Sign.NoSign,
            ast.LeftGuard(self.library, W, ast.Relation.Equal),
            head.function,
            body_elems,
            None,
        )

        # Preserve the original body and append the equality-to-aggregate literal.
        new_body: list[BodyLiteralAST] = list(st.body) + [body_agg]

        # Return the rewritten rule.
        return ast.StatementRule(self.library, st.location, new_head, new_body)

    def _error(
        self, location: Location, message: str, information: type | None = None
    ) -> None:
        """
        Record a syntactic error at the given location with a structured message.
        """
        self.errors.append(SyntacticError(location, message, information))
