from functools import singledispatchmethod
from typing import Any, Iterable

from clingo import ast

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree.types import SymbolSignature
from funasp.util.ast import function_arguments_ast


def _restore_literal(
    context: RewriteContext,
    node: ast.LiteralSymbolic,
) -> ast.LiteralComparison | None:
    """Restore prefixed function literals into equalities for non-evaluable signatures."""
    atom = node.atom
    if not isinstance(atom, (ast.TermFunction, ast.TermSymbolic)):
        return None  # pragma: no cover #TODO: Need to consider if this is the right approach

    prefix = context.prefix_function
    prefixed_name, arguments = function_arguments_ast(context.lib.library, atom)
    if not prefixed_name.startswith(prefix):
        return None

    base_name = prefixed_name[len(prefix) :]
    assert base_name is not None, "Base name should be non-empty after prefix removal"

    arguments = list(arguments)
    assert len(arguments) >= 1, "At least one argument is required for restoration"
    # if len(arguments) < 1:
    #     return None

    original_arity = len(arguments) - 1
    if SymbolSignature(base_name, original_arity) in context.evaluable_functions:
        return None

    left = ast.TermFunction(
        context.lib.library,
        node.location,
        base_name,
        [ast.ArgumentTuple(context.lib.library, arguments[:-1])],
    )
    right = ast.RightGuard(
        context.lib.library,
        ast.Relation.Equal,
        arguments[-1],
    )
    return ast.LiteralComparison(
        context.lib.library,
        node.location,
        node.sign,
        left,
        [right],
    )


class _RestoreNonEvaluableFunctionsTransformer:
    """Restore non-evaluable prefixed literals throughout a statement AST."""

    def __init__(self, context: RewriteContext) -> None:
        """Initialize the transformer with the rewrite context."""
        self.context = context
        self.library = context.lib.library

    @singledispatchmethod
    def dispatch(self, node: Any) -> Any:  # pragma: no cover
        """Dispatch restoration recursively across AST nodes."""
        return node.transform(self.library, self.dispatch) or node

    @dispatch.register
    def _(
        self, node: ast.LiteralSymbolic
    ) -> ast.LiteralComparison | ast.LiteralSymbolic:
        """Restore a protected prefixed literal when it maps to a non-evaluable function."""
        return _restore_literal(self.context, node) or node

    # @dispatch.register
    # def _(self, node: ast.HeadSimpleLiteral) -> ast.HeadSimpleLiteral:
    #     """Keep simple rule heads unchanged.

    #     Restoring to comparisons is only valid in body literals for this transformer.
    #     """
    #     return node

    # @dispatch.register
    # def _(self, node: ast.HeadDisjunction) -> ast.HeadDisjunction:
    #     """Keep disjunctive heads unchanged during non-evaluable restoration."""
    #     return node

    def rewrite(self, statement: ast.Statement) -> ast.Statement:
        """Apply restoration to one clingo statement."""
        rewritten = statement.transform(self.library, self.dispatch) or statement
        assert isinstance(rewritten, ast.Statement)
        return rewritten


def restore_non_evaluable_functions(
    context: RewriteContext,
    statement: ast.Statement,
) -> ast.Statement:
    """Restore non-evaluable prefixed function literals in a statement."""
    return _RestoreNonEvaluableFunctionsTransformer(context).rewrite(statement)


def restore_non_evaluable_functions_list(
    context: RewriteContext,
    statements: Iterable[ast.Statement],
) -> list[ast.Statement]:
    """Restore non-evaluable prefixed function literals in a statement sequence."""
    return [
        restore_non_evaluable_functions(context, statement) for statement in statements
    ]
