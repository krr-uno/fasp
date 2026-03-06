from functools import singledispatchmethod

from clingo.core import Library

from clingo import ast
from casp.util.util import negate_operator, is_constraint
from casp.util.ast import BodyLiteralAST, StatementAST
class NotAggregateConstraintTransformer:
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
        return rule
    
    @rewrite_rule.register
    def _(self, rule: ast.StatementRule) -> ast.StatementRule | None:
        # Only process constraints
        if is_constraint(rule.head):
            
            new_body: list[BodyLiteralAST] = []
            for lit in rule.body:
                if (isinstance(lit, ast.BodyAggregate)
                    and lit.sign == ast.Sign.Single
                    and lit.left is not None):
                    # Negated left guard if present
                    new_left = ast.LeftGuard(self.lib,
                        lit.left.term,
                        negate_operator(lit.left.relation)
                    )
                    # Rebuild ast.BodyAggregate with updated left guard and no negation
                    new_agg = ast.BodyAggregate(
                        self.lib,
                        lit.location,
                        ast.Sign.NoSign,
                        new_left,
                        lit.function,
                        lit.elements,
                        lit.right
                    )
                    new_body.append(new_agg)
                else:
                    new_body.append(lit)
            return ast.StatementRule(
                self.lib,
                rule.location,
                rule.head,
                new_body
            )
        else:
            return None