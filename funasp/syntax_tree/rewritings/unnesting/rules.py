from functools import singledispatchmethod
from typing import Any, List, Set

from clingo import ast
from clingo.core import Library

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import (
    AssignmentAggregateElement,
    AssignmentRule,
    FASP_Statement,
    HeadAggregateAssignment,
    HeadAggregateAssignmentElement,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from funasp.syntax_tree.collectors import collect_variables
from funasp.syntax_tree.rewritings.unnesting.literals import (
    UnnestFunctionsInLiteralsTransformer,
    unnest_functions,
)
from funasp.syntax_tree.types import SymbolSignature
from funasp.util.ast import (
    FreshVariableGenerator,
)
from funasp.util.iterables import map_none


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
        """Initialize the rule-level unnesting transformer."""
        self.lib = lib
        self.evaluable_functions = evaluable_functions

    def transform_rule(self, node: FASP_Statement) -> FASP_Statement:
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
        # print("Original rule:", node)
        return self._rewrite(node, var_gen)

    @singledispatchmethod
    def _rewrite_literal[T: (
        ast.BodyLiteral,
        ast.HeadLiteral,
    )](self, node: T, var_gen: FreshVariableGenerator) -> T | None:
        """Default: return node unchanged."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
        return node.transform(self.lib, self._rewrite_literal, var_gen)

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodySimpleLiteral,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodySimpleLiteral | ast.BodyConditionalLiteral | None:
        """Rewrite a simple body literal, turning negated rewrites into conditional literals."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
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
    ) -> ast.BodyConditionalLiteral | None:
        """Rewrite a conditional literal and append any generated comparisons to its condition."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
        update = {}
        literal = self.body_literal_transformer.unnest(node.literal)
        if literal is not None:
            update["literal"] = literal
        condition = []
        local_comps: List[ast.LiteralComparison] = []
        is_new_condition = False
        for cond in node.condition:
            new_cond, comps = unnest_functions(
                self.lib,
                cond,
                self.evaluable_functions,
                var_gen,
                allowed_in_negated_literals=False,
            )
            if new_cond is not None:
                is_new_condition = True
                condition.append(new_cond)
                local_comps.extend(comps)
            else:
                condition.append(cond)
        if is_new_condition or local_comps:
            condition.extend(local_comps)
            update["condition"] = condition
        return node.update(self.lib, **update) if update else None

    # Aggregates
    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodyAggregate | ast.HeadAggregate | HeadAggregateAssignment,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodyAggregate | ast.HeadAggregate | HeadAggregateAssignment:
        """Rewrite aggregate nodes by unnesting their elements and guards."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
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
            | AssignmentAggregateElement
        ),
        var_gen: FreshVariableGenerator,
    ) -> (
        ast.BodyAggregateElement
        | ast.HeadAggregateElement
        | HeadAggregateAssignmentElement
        | AssignmentAggregateElement
    ):
        """Rewrite aggregate elements by unnesting tuples, conditions, and embedded assignments."""
        # print(f"Visiting (Element) literal of type {node.__class__}: {node}")
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.evaluable_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update: dict[str, Any] = {}
        if isinstance(
            node,
            ast.BodyAggregateElement
            | ast.HeadAggregateElement
            | HeadAggregateAssignmentElement,
        ) and (
            tuple_ := map_none(lambda t: transformer.unnest(t, outer=False), node.tuple)
        ):
            update["tuple"] = tuple_
        if condition := map_none(
            lambda c: transformer.unnest(c, outer=False), node.condition
        ):
            update["condition"] = condition

        if isinstance(node, ast.HeadAggregateElement):
            literal = transformer.unnest(node.literal)
            if literal is not None:
                update["literal"] = literal
        elif isinstance(
            node, HeadAggregateAssignmentElement | AssignmentAggregateElement
        ):
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
        """Rewrite an optimize element and append any generated comparisons to its condition."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
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
        """Reject unreduced assignment aggregates during rule unnesting."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
        assert (
            False
        ), "HeadAggregateAssignment seen during function unnesting. This should not happen."

    @_rewrite_literal.register(ast.HeadSimpleLiteral | HeadSimpleAssignment)
    def _[T: (
        ast.HeadSimpleLiteral,
        HeadSimpleAssignment,
    )](self, node: T, var_gen: FreshVariableGenerator) -> T | None:
        """Rewrite a simple head node by unnesting evaluable functions within it."""
        # print(f"Visiting literal of type {node.__class__}: {node}")
        result = self.head_literal_transformer.unnest(node)
        # print(f"Result of unnesting head literal: {result}")
        return result if result is not None else node

    @singledispatchmethod
    def _rewrite(
        self, node: FASP_Statement, _: FreshVariableGenerator
    ) -> FASP_Statement:  # pragma: no cover
        """Default: return node unchanged."""
        return node

    # Rule Statements
    @_rewrite.register(ast.StatementRule | AssignmentRule)
    def _[T: (
        ast.StatementRule,
        AssignmentRule,
    )](self, node: T, var_gen: FreshVariableGenerator) -> T:
        """Rewrite a rule statement and append any residual comparisons to its body."""
        # print("Rewriting rule:", node)
        # if isinstance(node.head, ast.HeadSimpleLiteral | HeadSimpleAssignment):
        #     new_head = self.head_literal_transformer.unnest(node.head)
        # else:
        new_head = self._rewrite_literal(node.head, var_gen)
        # print("New head after unnesting:", new_head)

        new_body_literals: List[ast.BodyLiteral] = []

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
        # print("Updated rule:", node, "to", node.update(self.lib, **update))
        return node.update(self.lib, **update)

    @_rewrite.register(ast.StatementOptimize)
    def _(
        self, node: ast.StatementOptimize, var_gen: FreshVariableGenerator
    ) -> ast.StatementOptimize:
        """Rewrite all optimize elements in an optimize statement."""
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
        """Rewrite a weak constraint by unnesting its tuple and body literals."""
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

        new_body_literals: List[ast.BodyLiteral] = []
        are_new_body_literals = False

        for lit in node.body:
            new_lit = transformer.unnest(lit)
            if new_lit is None:
                new_body_literals.append(lit)
            else:
                new_body_literals.append(new_lit)
                are_new_body_literals = True

        new_body_literals_from_comps: List[ast.BodyLiteral] = []

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


def unnest(
    context: RewriteContext,
    statement: FASP_Statement,
) -> FASP_Statement:
    """Unnest evaluable functions in a single FASP statement."""
    collect_variables(statement)
    transformer = RuleRewriteTransformer(
        context.lib.library, context.evaluable_functions
    )
    return transformer.transform_rule(statement) or statement
