from collections.abc import Iterable
from functools import singledispatchmethod
from typing import List, Set

from clingo import ast
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    FASP_AST,
    AssignmentRule,
    HeadAggregateAssignment,
    HeadSimpleAssignment,
)
from fasp.syntax_tree.collectors import SymbolSignature, collect_variables
from fasp.syntax_tree.rewritings.unnesting.unnesting import (
    UnnestFunctionsInLiteralsTransformer,
    unnest_functions,
)
from fasp.util.ast import (
    BodyLiteralAST,
    FreshVariableGenerator,
    HeadLiteralAST,
)

# mypy: ignore-errors


def map_none[T](
    func,
    lst: Iterable[T],
) -> list[T] | None:
    """Applies `func` to each item in `lst`, returning a list of results.
    If all results are None, returns None.
    Otherwise, returns a list of non-None results.
    None results are replaced by the item itself.
    """
    all_none = True
    new_list = []
    for item in lst:
        result = func(item)
        if result is not None:
            new_list.append(result)
            all_none = False
        else:
            new_list.append(item)
    return new_list if not all_none else None


class RuleRewriteTransformer:
    """
    Performs rule-level rewriting by applying `unnest_functions`
    across all relevant AST substructures.
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions

    def transform_rule(self, node: FASP_AST) -> FASP_AST:
        """
        Entrypoint for rewriting an entire statement.
        """
        used = collect_variables(node)
        var_gen = FreshVariableGenerator(used)
        self.residual_comps: List[ast.LiteralComparison] = []
        self.head_literal_transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.evaluable_functions,
            var_gen,
            unnest_left_guard_equality=True,
        )
        self.body_literal_transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib, self.evaluable_functions, var_gen
        )
        return self._rewrite(node, var_gen)

    @singledispatchmethod
    def _rewrite_literal(
        self, node: BodyLiteralAST | HeadLiteralAST, _: FreshVariableGenerator
    ) -> tuple[FASP_AST, List[ast.LiteralComparison]]:
        """Default: return node unchanged."""
        print(f"Unnesting literal {node} ({type(node)})")
        return node

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodySimpleLiteral,
        var_gen: FreshVariableGenerator,
    ) -> tuple[ast.BodySimpleLiteral, List[ast.LiteralComparison]]:
        if node.literal.sign != ast.Sign.Single:
            literal = self.body_literal_transformer._unnest(node.literal)
            if literal is None:
                return node
            return node.update(self.lib, literal=literal)
        else:
            literal, comparisons = unnest_functions(
                self.lib, node.literal, self.evaluable_functions, var_gen
            )
            if not comparisons:
                return node
            false_lit = ast.LiteralBoolean(
                self.lib, literal.location, ast.Sign.NoSign, False
            )
            literal = literal.update(self.lib, sign=ast.Sign.NoSign)
            condition = [literal, *comparisons]
            return ast.BodyConditionalLiteral(
                self.lib, literal.location, false_lit, condition
            )

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodyConditionalLiteral,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodyConditionalLiteral:
        literal = self.body_literal_transformer._unnest(node.literal)

        condition = []
        local_comps: List[ast.LiteralComparison] = []
        for cond in node.condition:
            cond, comps = unnest_functions(
                self.lib,
                cond,
                self.evaluable_functions,
                var_gen,
                allowed_in_negated_literals=False,
            )
            condition.append(cond)
            local_comps.extend(comps)
        condition.extend(local_comps)
        return node.update(self.lib, literal=literal, condition=condition)

    # Aggregates
    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodyAggregate | ast.HeadAggregate,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodyAggregate | ast.HeadAggregate:
        new_elements = []
        for elem in node.elements:
            new_elem = self._rewrite_literal(elem, var_gen)
            new_elements.append(new_elem)

        new_left = (
            self.body_literal_transformer._unnest(node.left, outer=False)
            if node.left
            else None
        )
        new_right = (
            self.body_literal_transformer._unnest(node.right, outer=False)
            if node.right
            else None
        )

        # self.residual_comps = left_guard_comps + right_guard_comps
        return node.update(
            self.lib,
            left=new_left if new_left is not None else node.left,
            right=new_right if new_right is not None else node.right,
            elements=new_elements,
        )

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodyAggregateElement | ast.HeadAggregateElement,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodyAggregateElement | ast.HeadAggregateElement:

        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.evaluable_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update = {}
        if tuple_ := map_none(
            lambda t: transformer._unnest(t, outer=False), node.tuple
        ):
            update["tuple"] = tuple_
        if condition := map_none(
            lambda c: transformer._unnest(c, outer=False), node.condition
        ):
            update["condition"] = condition

        if isinstance(node, ast.HeadAggregateElement):
            literal = transformer._unnest(node.literal)
            if literal is not None:
                update["literal"] = literal
        if extra := transformer.pop_all_unnested_functions():
            condition = condition or node.condition
            condition.extend(extra)
            update["condition"] = condition
        return node.update(self.lib, **update)

    @_rewrite_literal.register
    def _(
        self, node: HeadAggregateAssignment, var_gen: FreshVariableGenerator
    ) -> HeadAggregateAssignment:
        assert (
            False
        ), "HeadAggregateAssignment seen during function unnesting. This should not happen."

    @singledispatchmethod
    def _rewrite(self, node: FASP_AST, _: FreshVariableGenerator) -> FASP_AST:
        """Default: return node unchanged."""
        return node

    # Rule Statements
    @_rewrite.register
    def _(
        self, node: ast.StatementRule | AssignmentRule, var_gen: FreshVariableGenerator
    ) -> ast.StatementRule:
        if isinstance(node.head, ast.HeadSimpleLiteral | HeadSimpleAssignment):
            new_head = self.head_literal_transformer._unnest(node.head) or node.head
        else:
            new_head = self._rewrite_literal(node.head, var_gen)

        new_body_literals: List[BodyLiteralAST] = []

        for lit in node.body:
            new_lit = self._rewrite_literal(lit, var_gen)

            # For Mypy
            # assert isinstance(new_lit, BodyLiteralAST)
            new_body_literals.append(new_lit)

        for comp in self.head_literal_transformer.pop_all_unnested_functions():
            new_body_literals.append(ast.BodySimpleLiteral(self.lib, literal=comp))

        for comp in self.body_literal_transformer.pop_all_unnested_functions():
            new_body_literals.append(ast.BodySimpleLiteral(self.lib, literal=comp))
        return node.update(self.lib, head=new_head, body=new_body_literals)
