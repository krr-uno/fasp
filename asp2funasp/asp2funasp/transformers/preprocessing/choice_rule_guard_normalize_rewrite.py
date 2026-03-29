from __future__ import annotations

from functools import singledispatchmethod
from typing import Optional, Tuple

import clingo.ast as ast
import clingo.core as core
from clingo.symbol import Number

import asp2funasp.util.util as util
from asp2funasp.transformers.preprocessing.base import PreprocessingTransformer
from asp2funasp.util.ast import HeadLiteralAST, StatementAST


class ChoiceGuardTransformer(PreprocessingTransformer):
    """Normalize guards in head choice aggregates for clingo 6 ASTs."""

    def __init__(self, lib: core.Library):
        self._lib = lib

    @singledispatchmethod
    def rewrite_rule(self, rule: StatementAST) -> StatementAST | None:
        return rule  # pragma: no cover

    @rewrite_rule.register
    def _(self, rule: ast.StatementRule) -> ast.StatementRule | None:
        head = rule.head
        if not self._is_choice_head(head):
            return None

        assert isinstance(
            head, (ast.HeadSetAggregate, ast.HeadAggregate)
        )  # Redundant for mypy: typecheck
        left_guard = head.left
        right_guard = head.right

        left_guard, left_changed = self._normalize_guard(left_guard, is_left=True)
        right_guard, right_changed = self._normalize_guard(right_guard, is_left=False)
        left_guard, right_guard, collapse_changed = util.collapse_equal_bounds(
            self._lib, left_guard, right_guard
        )
        if (
            right_guard
            and right_guard.relation == ast.Relation.NotEqual
            and left_guard == None
        ):
            left_guard = ast.LeftGuard(
                self._lib, right_guard.term, right_guard.relation
            )
            right_guard = None
            right_changed = True
            left_changed = True

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
