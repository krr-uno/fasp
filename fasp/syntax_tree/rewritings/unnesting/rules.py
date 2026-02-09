from functools import singledispatchmethod
from typing import Any, List, Set

from clingo import ast
from clingo.core import Library

from fasp.syntax_tree._nodes import (
    FASP_AST,
    AssignmentRule,
    HeadAggregateAssignment,
    HeadAggregateAssignmentElement,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from fasp.syntax_tree.collectors import SymbolSignature, collect_variables
from fasp.syntax_tree.rewritings.unnesting.literals import (
    UnnestFunctionsInLiteralsTransformer,
    unnest_functions,
)
from fasp.util.ast import (
    BodyLiteralAST,
    FreshVariableGenerator,
    HeadLiteralAST,
)
from fasp.util.iterables import map_none


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
    def _rewrite_literal[T: (
        BodyLiteralAST,
        HeadLiteralAST,
    )](self, node: T, var_gen: FreshVariableGenerator) -> T | None:
        """Default: return node unchanged."""
        return node.transform(self.lib, self._rewrite_literal, var_gen)

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodySimpleLiteral,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodySimpleLiteral | ast.BodyConditionalLiteral | None:
        if node.literal.sign != ast.Sign.Single:
            literal = self.body_literal_transformer.unnest(node.literal)
            if literal is None:
                return None
            return node.update(self.lib, literal=literal)
        else:
            literal, comparisons = unnest_functions(
                self.lib, node.literal, self.evaluable_functions, var_gen
            )
            if not comparisons:
                return None

            assert isinstance(
                literal,
                ast.LiteralBoolean | ast.LiteralComparison | ast.LiteralSymbolic,
            )
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
        update = {}
        literal = self.body_literal_transformer.unnest(node.literal)
        if literal is not None:
            update["literal"] = literal
        condition = []
        local_comps: List[ast.LiteralComparison] = []
        for cond in node.condition:
            new_cond, comps = unnest_functions(
                self.lib,
                cond,
                self.evaluable_functions,
                var_gen,
                allowed_in_negated_literals=False,
            )
            # print(new_cond, list(map(str,comps)), self.evaluable_functions)
            condition.append(new_cond)
            local_comps.extend(comps)
        # update["condition"] = condition
        condition.extend(local_comps)
        if condition not in ([None], [], None):
            update["condition"] = condition
        return node.update(self.lib, **update)

    # Aggregates
    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodyAggregate | ast.HeadAggregate | HeadAggregateAssignment,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodyAggregate | ast.HeadAggregate | HeadAggregateAssignment:
        new_elements = []
        for elem in node.elements:
            new_elem = self._rewrite_literal(elem, var_gen)
            new_elements.append(new_elem)

        new_left = (
            self.body_literal_transformer.unnest(node.left, outer=False)
            if node.left
            else None
        )
        new_right = (
            self.body_literal_transformer.unnest(node.right, outer=False)
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
        node: (
            ast.BodyAggregateElement
            | ast.HeadAggregateElement
            | HeadAggregateAssignmentElement
        ),
        var_gen: FreshVariableGenerator,
    ) -> (
        ast.BodyAggregateElement
        | ast.HeadAggregateElement
        | HeadAggregateAssignmentElement
    ):

        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.evaluable_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update: dict[str, Any] = {}
        if tuple_ := map_none(lambda t: transformer.unnest(t, outer=False), node.tuple):
            update["tuple"] = tuple_
        if condition := map_none(
            lambda c: transformer.unnest(c, outer=False), node.condition
        ):
            update["condition"] = condition

        if isinstance(node, ast.HeadAggregateElement):
            literal = transformer.unnest(node.literal)
            if literal is not None:
                update["literal"] = literal
        elif isinstance(node, HeadAggregateAssignmentElement):
            assignment = transformer.unnest(node.assignment)
            if assignment is not None:
                update["assignment"] = assignment

        if extra := transformer.pop_all_unnested_functions():
            condition = condition or list(node.condition)
            condition.extend(extra)
            update["condition"] = condition
        return node.update(self.lib, **update)

    @_rewrite_literal.register
    def _(
        self,
        node: ast.OptimizeElement,
        var_gen: FreshVariableGenerator,
    ) -> ast.OptimizeElement:
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.evaluable_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update: dict[str, Any] = {}
        tuple = transformer.unnest(node.tuple)
        if tuple is not None:
            update["tuple"] = tuple
        if condition := map_none(
            lambda c: transformer.unnest(c, outer=False), node.condition
        ):
            update["condition"] = condition
        if extra := transformer.pop_all_unnested_functions():
            condition = condition or list(node.condition)
            condition.extend(extra)
            update["condition"] = condition
        return node.update(self.lib, **update)

    @_rewrite_literal.register
    def _(
        self, node: HeadAssignmentAggregate, var_gen: FreshVariableGenerator
    ) -> HeadAssignmentAggregate:
        assert (
            False
        ), "HeadAggregateAssignment seen during function unnesting. This should not happen."

    @singledispatchmethod
    def _rewrite(
        self, node: FASP_AST, _: FreshVariableGenerator
    ) -> FASP_AST:  # pragma: no cover
        """Default: return node unchanged."""
        return node

    # Rule Statements
    @_rewrite.register(ast.StatementRule | AssignmentRule)
    def _[T: (
        ast.StatementRule,
        AssignmentRule,
    )](self, node: T, var_gen: FreshVariableGenerator) -> T:
        if isinstance(node.head, ast.HeadSimpleLiteral | HeadSimpleAssignment):
            new_head = self.head_literal_transformer.unnest(node.head)
        else:
            new_head = self._rewrite_literal(node.head, var_gen)

        new_body_literals: List[BodyLiteralAST] = []

        are_new_body_literals = False
        for lit in node.body:
            new_lit = self._rewrite_literal(lit, var_gen)
            if new_lit is None:
                new_body_literals.append(lit)
            else:
                new_body_literals.append(new_lit)
                are_new_body_literals = True

        if not new_head and not are_new_body_literals:
            return node

        for comp in self.head_literal_transformer.pop_all_unnested_functions():
            new_body_literals.append(ast.BodySimpleLiteral(self.lib, literal=comp))

        for comp in self.body_literal_transformer.pop_all_unnested_functions():
            new_body_literals.append(ast.BodySimpleLiteral(self.lib, literal=comp))

        update = {}
        if new_head:
            update["head"] = new_head
        if new_body_literals:
            update["body"] = new_body_literals
        return node.update(self.lib, **update)

    @_rewrite.register(ast.StatementOptimize)
    def _(
        self, node: ast.StatementOptimize, var_gen: FreshVariableGenerator
    ) -> ast.StatementOptimize:
        new_elements = []
        for elem in node.elements:
            new_elem = self._rewrite_literal(elem, var_gen)
            new_elements.append(new_elem)

        return node.update(
            self.lib,
            elements=new_elements,
        )

    @_rewrite.register(ast.StatementWeakConstraint)
    def _(
        self, node: ast.StatementWeakConstraint, var_gen: FreshVariableGenerator
    ) -> ast.StatementWeakConstraint:
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.evaluable_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update: dict[str, Any] = {}
        tuple = transformer.unnest(node.tuple)
        if tuple is not None:
            update["tuple"] = tuple
        comps_1 = transformer.pop_all_unnested_functions()

        new_body_literals: List[BodyLiteralAST] = []
        are_new_body_literals = False

        for lit in node.body:
            new_lit = transformer.unnest(lit)
            if new_lit is None:
                new_body_literals.append(lit)
            else:
                new_body_literals.append(new_lit)
                are_new_body_literals = True

        new_body_literals_from_comps: List[BodyLiteralAST] = []

        if are_new_body_literals:
            new_body_literals_from_comps.extend(new_body_literals)
        else:
            new_body_literals_from_comps.extend(node.body)
        comps_2 = transformer.pop_all_unnested_functions()

        if comps_1:
            new_body_literals_from_comps.extend(
                map(lambda c: ast.BodySimpleLiteral(self.lib, c), comps_1)
            )

        if comps_2:
            new_body_literals_from_comps.extend(
                map(lambda c: ast.BodySimpleLiteral(self.lib, c), comps_2)
            )

        update["body"] = new_body_literals_from_comps

        return node.update(self.lib, **update)
