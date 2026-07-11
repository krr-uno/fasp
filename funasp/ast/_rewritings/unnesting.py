"""
Unnesting of intensional functions over the prefixed clingo representation.

A term ``f(t)`` whose signature is intensional and which occurs in a nested
position is replaced by a fresh ``FUN`` variable, and the equality
``f(t) = FUN`` is appended to the enclosing body or condition. The
term-level logic lives in ``funasp.ast._rewritings.literals``; this module
provides the statement-level driver over plain clingo nodes (heads are
already prefixed atoms).
"""

from functools import singledispatchmethod
from typing import Any, List, Set

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.ast._rewritings.context import RewriteContext
from funasp.ast._rewritings.literals import (
    UnnestFunctionsInLiteralsTransformer,
    unnest_functions,
)
from funasp.util.ast import FreshVariableGenerator, RewritingException, SemanticError
from funasp.util.collectors import collect_variables
from funasp.util.iterables import map_none
from funasp.util.types import SymbolSignature


class StatementUnnestTransformer:
    """
    Performs statement-level rewriting by applying `unnest_functions`
    across all relevant AST substructures.
    """

    def __init__(
        self,
        lib: Library,
        intensional_functions: Set[SymbolSignature],
    ):
        """Initialize the statement-level unnesting transformer."""
        self.lib = lib
        self.intensional_functions = intensional_functions

    def transform_statement(self, node: ast.Statement) -> ast.Statement:
        """
        Entrypoint for rewriting an entire statement.
        """
        used = collect_variables(node)
        var_gen = FreshVariableGenerator(used)
        self.head_literal_transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.intensional_functions,
            var_gen,
            unnest_left_guard_equality=True,
        )
        self.body_literal_transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib, self.intensional_functions, var_gen
        )
        return self._rewrite(node, var_gen)

    @singledispatchmethod
    def _rewrite_literal[
        T: (
            ast.BodyLiteral,
            ast.HeadLiteral,
        )
    ](
        self, node: T, var_gen: FreshVariableGenerator
    ) -> T | None:
        """Default: recurse into the node's children."""
        return node.transform(  # pragma: no cover - fallback for future AST nodes
            self.lib, self._rewrite_literal, var_gen
        )

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodySimpleLiteral,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodySimpleLiteral | ast.BodyConditionalLiteral | None:
        """Rewrite a simple body literal, turning negated rewrites into conditional literals."""
        if node.literal.sign != ast.Sign.Single:
            literal = self.body_literal_transformer.unnest(node.literal)
            if literal is None:
                return None
            return node.update(self.lib, literal=literal)
        else:
            literal, comparisons = unnest_functions(
                self.lib, node.literal, self.intensional_functions, var_gen
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
        update = {}
        # The comparisons generated for the main literal must stay inside the
        # condition: its variables may be local to the conditional literal.
        literal_transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib, self.intensional_functions, var_gen
        )
        literal = literal_transformer.unnest(node.literal)
        if literal is not None:
            update["literal"] = literal
        literal_comps = literal_transformer.pop_all_unnested_functions()
        condition = []
        local_comps: List[ast.LiteralComparison] = []
        is_new_condition = False
        for cond in node.condition:
            new_cond, comps = unnest_functions(
                self.lib,
                cond,
                self.intensional_functions,
                var_gen,
                allowed_in_negated_literals=False,
            )
            if new_cond is not None:
                is_new_condition = True
                condition.append(new_cond)
                local_comps.extend(comps)
            else:
                condition.append(cond)
        if is_new_condition or local_comps or literal_comps:
            condition.extend(local_comps)
            condition.extend(literal_comps)
            update["condition"] = condition
        return node.update(self.lib, **update) if update else None

    # Aggregates
    @_rewrite_literal.register
    def _(
        self,
        node: (
            ast.BodyAggregate
            | ast.BodySetAggregate
            | ast.HeadAggregate
            | ast.HeadSetAggregate
        ),
        var_gen: FreshVariableGenerator,
    ) -> (
        ast.BodyAggregate
        | ast.BodySetAggregate
        | ast.HeadAggregate
        | ast.HeadSetAggregate
        | None
    ):
        """Rewrite aggregate nodes by unnesting their elements and guards."""
        update: dict[str, Any] = {}
        if new_elements := map_none(
            lambda element: self._rewrite_literal(element, var_gen), node.elements
        ):
            update["elements"] = new_elements

        if node.left is not None:
            new_left = self.body_literal_transformer.unnest(node.left, outer=False)
            if new_left is not None:
                update["left"] = new_left
        if node.right is not None:
            new_right = self.body_literal_transformer.unnest(node.right, outer=False)
            if new_right is not None:
                update["right"] = new_right

        return node.update(self.lib, **update) if update else None

    @_rewrite_literal.register
    def _(
        self,
        node: ast.BodyAggregateElement | ast.HeadAggregateElement,
        var_gen: FreshVariableGenerator,
    ) -> ast.BodyAggregateElement | ast.HeadAggregateElement | None:
        """Rewrite aggregate elements by unnesting tuples, conditions, and literals."""
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.intensional_functions,
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

        if extra := transformer.pop_all_unnested_functions():
            condition = condition or list(node.condition)
            condition.extend(extra)
            update["condition"] = condition
        return node.update(self.lib, **update) if update else None

    @_rewrite_literal.register
    def _(
        self,
        node: ast.SetAggregateElement,
        var_gen: FreshVariableGenerator,
    ) -> ast.SetAggregateElement | None:
        """Rewrite a set aggregate element by unnesting its literal and condition."""
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.intensional_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update: dict[str, Any] = {}
        literal = transformer.unnest(node.literal)
        if literal is not None:
            update["literal"] = literal
        if condition := map_none(
            lambda c: transformer.unnest(c, outer=False), node.condition
        ):
            update["condition"] = condition
        if extra := transformer.pop_all_unnested_functions():
            condition = condition or list(node.condition)
            condition.extend(extra)
            update["condition"] = condition
        return node.update(self.lib, **update) if update else None

    @_rewrite_literal.register
    def _(
        self,
        node: ast.OptimizeElement,
        var_gen: FreshVariableGenerator,
    ) -> ast.OptimizeElement | None:
        """Rewrite an optimize element and append any generated comparisons to its condition."""
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.intensional_functions,
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
        return node.update(self.lib, **update) if update else None

    @_rewrite_literal.register
    def _(self, node: ast.HeadDisjunction, var_gen: FreshVariableGenerator) -> None:
        """Reject intensional function terms in disjunctive rule heads.

        Unnesting a term in a disjunct would require adding its lookup to the
        rule body, which changes the support conditions of every disjunct. The
        translation does not currently define that transformation, so reject
        it instead of letting clingo interpret the function as a Herbrand term.
        """
        del var_gen
        offending: ast.TermFunction | None = None
        offending_location = node.location

        def find_intensional_function(child: Any) -> None:
            nonlocal offending, offending_location
            if offending is not None:
                return
            if not isinstance(child, ast.TermFunction):
                child.visit(find_intensional_function)
                return
            if any(
                SymbolSignature(child.name, len(entry.arguments))
                in self.intensional_functions
                for entry in child.pool
            ):
                offending = child
                offending_location = child.location
                return
            child.visit(find_intensional_function)

        node.visit(find_intensional_function)
        if offending is not None:
            raise RewritingException(
                [
                    SemanticError(
                        offending_location,
                        "intensional functions are not allowed in disjunctive heads: "
                        f"'{offending}'",
                    )
                ]
            )
        return None

    @_rewrite_literal.register
    def _(
        self, node: ast.HeadSimpleLiteral, var_gen: FreshVariableGenerator
    ) -> ast.HeadSimpleLiteral | None:
        """Rewrite a simple head literal by unnesting intensional functions within it."""
        return self.head_literal_transformer.unnest(node)

    @singledispatchmethod
    def _rewrite(self, node: ast.Statement, _: FreshVariableGenerator) -> ast.Statement:
        """Default: return node unchanged."""
        return node

    # Rule Statements
    @_rewrite.register
    def _(
        self, node: ast.StatementRule, var_gen: FreshVariableGenerator
    ) -> ast.StatementRule:
        """Rewrite a rule statement and append any residual comparisons to its body."""
        new_head = self._rewrite_literal(node.head, var_gen)
        rewritten_body = map_none(
            lambda literal: self._rewrite_literal(literal, var_gen), node.body
        )
        comparisons = [
            *self.head_literal_transformer.pop_all_unnested_functions(),
            *self.body_literal_transformer.pop_all_unnested_functions(),
        ]

        update: dict[str, Any] = {}
        if new_head is not None:
            update["head"] = new_head
        if rewritten_body is not None or comparisons:
            body = rewritten_body or list(node.body)
            body.extend(
                ast.BodySimpleLiteral(self.lib, literal=comparison)
                for comparison in comparisons
            )
            update["body"] = body
        return node.update(self.lib, **update) if update else node

    @_rewrite.register
    def _(
        self, node: ast.StatementOptimize, var_gen: FreshVariableGenerator
    ) -> ast.StatementOptimize:
        """Rewrite all optimize elements in an optimize statement."""
        new_elements = map_none(
            lambda element: self._rewrite_literal(element, var_gen), node.elements
        )
        if new_elements is None:
            return node
        return node.update(self.lib, elements=new_elements)

    @_rewrite.register
    def _(
        self, node: ast.StatementWeakConstraint, var_gen: FreshVariableGenerator
    ) -> ast.StatementWeakConstraint:
        """Rewrite a weak constraint by unnesting its tuple and body literals."""
        transformer = UnnestFunctionsInLiteralsTransformer(
            self.lib,
            self.intensional_functions,
            var_gen,
            allowed_in_negated_literals=False,
        )
        update: dict[str, Any] = {}
        new_tuple = transformer.unnest(node.tuple)
        if new_tuple is not None:
            update["tuple"] = new_tuple
        tuple_comparisons = transformer.pop_all_unnested_functions()

        new_body = map_none(transformer.unnest, node.body)
        body_comparisons = transformer.pop_all_unnested_functions()
        comparisons = [*tuple_comparisons, *body_comparisons]
        if new_body is not None or comparisons:
            body = new_body or list(node.body)
            body.extend(
                ast.BodySimpleLiteral(self.lib, comparison)
                for comparison in comparisons
            )
            update["body"] = body

        return node.update(self.lib, **update) if update else node


def unnest_statement(
    context: RewriteContext,
    statement: ast.Statement,
) -> ast.Statement:
    """Unnest intensional functions in a single statement."""
    transformer = StatementUnnestTransformer(
        context.lib.library, context.intensional_functions
    )
    return transformer.transform_statement(statement)
