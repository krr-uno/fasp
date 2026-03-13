from functools import singledispatchmethod

from clingo import ast
from clingo.core import Library

from asp2fasp.transformers.preprocessing.base import PreprocessingTransformer
from asp2fasp.util.ast import BodyLiteralAST, StatementAST
from asp2fasp.util.util import collapse_equal_bounds, is_constraint, negate_operator


class NotAggregateConstraintTransformer(PreprocessingTransformer):
    """
    For each ast.StatementRule:
      - Scan its body for ast.BodyAggregate with ast.sign=ast.Sign.Single (negation).
      - Negate the left guard's operator.
      - Remove the negation sign.
    """

    def __init__(self, lib: Library):
        self.lib = lib

    @singledispatchmethod
    def rewrite_rule(self, rule: StatementAST) -> StatementAST | None:
        return rule  # pragma: no cover

    @rewrite_rule.register
    def _(self, rule: ast.StatementRule) -> ast.StatementRule | None:
        # Only process constraints
        if is_constraint(rule.head):

            new_body: list[BodyLiteralAST] = []
            for lit in rule.body:
                # For cases like `:- not #agg{} = N...` into `:- not N = #agg{}...`
                if isinstance(lit, ast.BodyAggregate):
                    left_guard, right_guard, collapse_changed = collapse_equal_bounds(
                        self.lib, lit.left, lit.right
                    )

                    if collapse_changed:
                        lit = lit.update(
                            self.lib, **{"left": left_guard, "right": right_guard}
                        )

                # Process normalized constraints
                if (
                    isinstance(lit, ast.BodyAggregate)
                    and lit.sign == ast.Sign.Single
                    and lit.left is not None
                ):
                    # Negated left guard if present
                    new_left = ast.LeftGuard(
                        self.lib, lit.left.term, negate_operator(lit.left.relation)
                    )
                    # Rebuild ast.BodyAggregate with updated left guard and no negation
                    new_agg = ast.BodyAggregate(
                        self.lib,
                        lit.location,
                        ast.Sign.NoSign,
                        new_left,
                        lit.function,
                        lit.elements,
                        lit.right,
                    )
                    new_body.append(new_agg)
                else:
                    new_body.append(lit)
            return ast.StatementRule(self.lib, rule.location, rule.head, new_body)
        else:
            return None
