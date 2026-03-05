import clingo
from clingo.core import Library

from clingo.ast import StatementRule, HeadSimpleLiteral, LiteralBoolean, Sign, BodyAggregate, StatementRule, LeftGuard
from functional_to_casp.util.util import negate_operator
from functional_to_casp.util.ast import BodyLiteralAST
class NotAggregateConstraintTransformer:
    """
    For each StatementRule:
      - Scan its body for BodyAggregate with sign=Sign.Single (negation).
      - Negate the left guard's operator.
      - Remove the negation sign.
    """
    def __init__(self, lib: Library):
        self.lib = lib

    def rewrite_rule(self, rule: StatementRule) -> StatementRule | None:
        # Only process constraints: head is a HeadSimpleLiteral with literal of type LiteralBoolean(sign = Single)
        if isinstance(rule.head, HeadSimpleLiteral):
            head = rule.head
            if isinstance(head, HeadSimpleLiteral) and isinstance(head.literal, LiteralBoolean) and head.literal.sign == Sign.NoSign and  head.literal.value == False:
                
                new_body: list[BodyLiteralAST] = []
                for lit in rule.body:
                    if (isinstance(lit, BodyAggregate)
                        and lit.sign == Sign.Single):
                        # Negate left guard if present
                        if lit.left is not None:
                            new_left = LeftGuard(self.lib,
                                lit.left.term,
                                negate_operator(lit.left.relation)
                            )
                        else:
                            new_left = None
                        # Rebuild BodyAggregate with updated left guard and no negation
                        new_agg = BodyAggregate(
                            self.lib,
                            lit.location,
                            Sign.NoSign,
                            new_left,
                            lit.function,
                            lit.elements,
                            lit.right
                        )
                        new_body.append(new_agg)
                    else:
                        new_body.append(lit)
                return StatementRule(
                    self.lib,
                    rule.location,
                    rule.head,
                    new_body
                )
            else:
                return None
        else:
            return None