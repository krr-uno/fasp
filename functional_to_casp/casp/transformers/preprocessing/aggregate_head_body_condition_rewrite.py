from functools import singledispatchmethod

import clingo.ast as ast
import clingo.core as core

from casp.transformers.preprocessing.base import PreprocessingTransformer
from casp.util.ast import HeadLiteralAST, StatementAST


class AggregateHeadBodyConditionTransformer(PreprocessingTransformer):
    """Move inline head-aggregate conditions into the surrounding body."""

    def __init__(self, lib: core.Library):
        self._lib = lib

    @singledispatchmethod
    def rewrite_rule(self, rule: StatementAST) -> StatementAST | None:
        return rule

    @rewrite_rule.register
    def _(self, rule: ast.StatementRule) -> ast.StatementRule | None:
        head = rule.head
        if not isinstance(head, (ast.HeadAggregate, ast.HeadSetAggregate)):
            return None
        if not self._is_guardless_head_aggregate(head):
            return None

        updated_elements = []
        lifted_literals = []

        for element in head.elements:
            if element.condition:
                for lit in element.condition:
                    lifted_literals.append(ast.BodySimpleLiteral(self._lib, lit))
                updated_elements.append(element.update(self._lib, condition=[]))
            else:
                updated_elements.append(element)

        if not lifted_literals:
            return None

        new_head = head.update(self._lib, elements=updated_elements)
        new_body = list(rule.body) + lifted_literals
        return rule.update(self._lib, head=new_head, body=new_body)

    @staticmethod
    def _is_guardless_head_aggregate(head: HeadLiteralAST) -> bool:
        left = getattr(head, "left", getattr(head, "left_guard", None))
        right = getattr(head, "right", getattr(head, "right_guard", None))
        return left is None and right is None
