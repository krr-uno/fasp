from __future__ import annotations

from functools import singledispatchmethod
from typing import Optional, Tuple

import clingo.ast as ast
import clingo.core as core
from clingo.symbol import Number

from casp.transformers.preprocessing.base import PreprocessingTransformer
from casp.util.ast import HeadLiteralAST, StatementAST


class ChoiceGuardTransformer(PreprocessingTransformer):
    """Normalize guards in head choice aggregates for clingo 6 ASTs."""

    def __init__(self, lib: core.Library):
        self._lib = lib

    @singledispatchmethod
    def rewrite_rule(self, rule: StatementAST) -> StatementAST | None:
        return rule

    @rewrite_rule.register
    def _(self, rule: ast.StatementRule) -> ast.StatementRule | None:
        head = rule.head
        if not self._is_choice_head(head):
            return None

        assert isinstance(
            head, (ast.HeadSetAggregate, ast.HeadAggregate)
        )  # Redunant for mypy: typecheck
        left_guard = head.left
        right_guard = head.right

        left_guard, left_changed = self._normalize_guard(left_guard, is_left=True)
        right_guard, right_changed = self._normalize_guard(right_guard, is_left=False)
        left_guard, right_guard, collapse_changed = self._collapse_equal_bounds(
            left_guard, right_guard
        )

        if not any([left_changed, right_changed, collapse_changed]):
            return None

        new_head = head.update(self._lib, **{"left": left_guard, "right": right_guard})
        return rule.update(self._lib, head=new_head)

    @staticmethod
    def _is_choice_head(head: HeadLiteralAST) -> bool:
        return isinstance(head, (ast.HeadSetAggregate, ast.HeadAggregate))

    def _normalize_guard[
        T: (
            ast.LeftGuard,
            ast.RightGuard,
        )
    ](
        self, guard: Optional[T], is_left: bool = True
    ) -> Tuple[Optional[T], bool]:
        if guard is None:
            return None, False
        if guard.relation != ast.Relation.Less:
            return guard, False
        term_symbol = getattr(guard.term, "symbol", None)
        if term_symbol is None:
            return guard, False
        delta = 1 if is_left else -1
        new_symbol = Number(self._lib, term_symbol.number + delta)
        new_term = guard.term.update(self._lib, symbol=new_symbol)
        new_guard = guard.update(
            self._lib, relation=ast.Relation.LessEqual, term=new_term
        )
        return new_guard, True

    def _collapse_equal_bounds(
        self,
        left: ast.LeftGuard | None,
        right: ast.RightGuard | None,
    ) -> Tuple[ast.LeftGuard | None, ast.RightGuard | None, bool]:
        if left is None or right is None:
            return left, right, False
        if not self._same_bound_symbol(left, right):
            return left, right, False
        new_right = right.update(self._lib, relation=ast.Relation.Equal)
        return None, new_right, True

    @staticmethod
    def _same_bound_symbol(left: ast.LeftGuard, right: ast.RightGuard) -> bool:
        l_sym = getattr(left.term, "symbol", None)
        r_sym = getattr(right.term, "symbol", None)
        return l_sym is not None and r_sym is not None and l_sym == r_sym


# import clingo

# class ChoiceGuardTransformer(clingo.ast.Transformer):
#     """
#     Normalize guards in choice-rule aggregates:
#       1) Turn `< { ... }` into `<= { ... }` by shift the number:
#          left-guard `< n`   → `<= n+1`
#          right-guard `m >`  → `>= m-1` (i.e. `<= m` becomes `<= m-1`)
#       2) If after shift both guards become equal constants, drop the left
#          guard and turn the right into `==`.
#     """
#     def visit_Rule(self, node: clingo.ast.Rule) -> clingo.ast.AST:
#         head = node.head

#         if head.ast_type != clingo.ast.ASTType.Aggregate:
#             return node

#         left_guard = head.left_guard
#         right_guard = head.right_guard

#         if left_guard:
#             try:
#                 # Check if the left guard operator is '<' and transform it
#                 if left_guard.comparison == clingo.ast.ComparisonOperator.LessThan:
#                     # Extract the integer value from the clingo.Symbol
#                     left_value = left_guard.term.symbol.number  # Extract the number from the Symbol

#                     # Add 1 and create a new clingo.Symbol with the updated value
#                     new_left_value = clingo.Number(left_value + 1)

#                     # Update the term and comparison operator directly
#                     left_guard.term = clingo.ast.SymbolicTerm(
#                         location=left_guard.term.location,  # Use the existing location
#                         symbol=new_left_value
#                     )
#                     left_guard.comparison = clingo.ast.ComparisonOperator.LessEqual
#                     # print("Choice Rule after left guard transformation:", str(rule))
#             except Exception as e:
#                 print(f"Error processing left guard in rule: {e}")
#                 import traceback
#                 print(traceback.format_exc())

#         if right_guard:
#             try:
#                 # Check if the right guard operator is '<' and transform it
#                 if right_guard.comparison == clingo.ast.ComparisonOperator.LessThan:
#                     # Extract the integer value from the clingo.Symbol
#                     right_value = right_guard.term.symbol.number  # Extract the number from the Symbol

#                     # Subtract 1 and create a new clingo.Symbol with the updated value
#                     new_right_value = clingo.Number(right_value - 1)

#                     # Update the term and comparison operator directly
#                     right_guard.term = clingo.ast.SymbolicTerm(
#                         location=right_guard.term.location,  # Use the existing location
#                         symbol=new_right_value
#                     )
#                     right_guard.comparison = clingo.ast.ComparisonOperator.LessEqual
#                     # print(elem.items())
#                     # print("Choice Rule after right guard transformation:", str(rule))
#             except Exception as e:
#                 print(f"Error processing right guard in rule: {e}")
#                 import traceback
#                 print(traceback.format_exc())

#         if left_guard and right_guard:
#             lnum = left_guard.term.symbol.number
#             rnum = right_guard.term.symbol.number
#             if lnum == rnum:
#                 right_guard = None
#                 left_guard = clingo.ast.Guard(clingo.ast.ComparisonOperator.Equal, left_guard.term)
#         # rebuild the head aggregate with updated guards
#         new_head = clingo.ast.Aggregate(
#             location=head.location,
#             elements=head.elements,
#             left_guard=left_guard,
#             right_guard=right_guard,
#         )

#         # rebuild the rule
#         return clingo.ast.Rule(node.location, new_head, node.body)
