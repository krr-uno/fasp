"""
Restoration of non-evaluable prefixed function literals.

The comparisons step prefixes a pooled equality ``f(a;b,c) = V`` whenever any
of its candidate arities is evaluable. After clingo's rewriting unpools the
statements, the entries whose arity is *not* evaluable must be restored to
plain comparisons (e.g. ``Ff(b,c,V)`` back to ``f(b,c) = V`` when only ``f/1``
is evaluable).
"""

from functools import singledispatchmethod
from typing import Any

from clingo_funasp import ast
from clingo_funasp.symbol import SymbolType

from funasp.rewriting._context import RewriteContext
from funasp.rewriting.types import SymbolSignature
from funasp.util.ast import function_arguments_ast


def _restore_literal(
    context: RewriteContext,
    node: ast.LiteralSymbolic,
) -> ast.LiteralComparison | None:
    """Restore prefixed function literals into equalities for non-evaluable signatures."""
    atom = node.atom
    if isinstance(atom, ast.TermFunction):
        prefixed_name = atom.name
    elif isinstance(atom, ast.TermSymbolic) and atom.symbol.type == SymbolType.Function:
        prefixed_name = atom.symbol.name
    else:
        # TODO: Need to add test for this
        return None  # pragma: no cover

    prefix = context.prefix_function
    if not prefixed_name.startswith(prefix):
        return None

    base_name, arguments = function_arguments_ast(context.lib.library, atom)
    base_name = base_name[len(prefix) :]
    assert base_name is not None

    arguments = list(arguments)
    assert len(arguments) >= 1

    original_arity = len(arguments) - 1
    if SymbolSignature(base_name, original_arity) in context.evaluable_functions:
        return None

    left = ast.TermFunction(
        context.lib.library,
        node.location,
        base_name,
        [ast.ArgumentTuple(context.lib.library, arguments[:-1])],
    )
    right_arg = (
        arguments[-1]
        if isinstance(arguments[-1], ast.Term)
        else ast.TermVariable(context.lib.library, node.location, "_", anonymous=True)
    )
    right = ast.RightGuard(
        context.lib.library,
        ast.Relation.Equal,
        right_arg,
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
    def dispatch(self, node: Any) -> Any | None:  # pragma: no cover
        """Dispatch restoration recursively across AST nodes."""
        return node.transform(self.library, self.dispatch)

    @dispatch.register
    def _(self, node: ast.LiteralSymbolic) -> ast.LiteralComparison | None:
        """Restore a prefixed literal when it maps to a non-evaluable function."""
        return _restore_literal(self.context, node)

    @dispatch.register
    def _(self, node: ast.StatementRule) -> ast.StatementRule | None:
        """Restore prefixed literals in both rule heads and bodies."""
        rewritten_head = self.dispatch(node.head)
        if rewritten_head is None:
            new_head = node.head
            head_changed = False
        else:
            assert isinstance(rewritten_head, (ast.HeadLiteral, ast.HeadDisjunction))
            new_head = rewritten_head
            head_changed = True

        new_body: list[ast.BodyLiteral] = []
        changed = head_changed
        for literal in node.body:
            rewritten = self.dispatch(literal)
            if rewritten is None:
                new_body.append(literal)
            else:
                assert isinstance(rewritten, ast.BodyLiteral)
                new_body.append(rewritten)
                changed = True

        if not changed:
            return None
        return ast.StatementRule(self.library, node.location, new_head, new_body)

    def rewrite(self, statement: ast.Statement) -> ast.Statement:
        """Apply restoration to one clingo statement."""
        rewritten = self.dispatch(statement)
        if rewritten is None:
            return statement
        assert isinstance(rewritten, ast.Statement)
        return rewritten


def restore_non_evaluable_functions(
    context: RewriteContext,
    statement: ast.Statement,
) -> ast.Statement:
    """Restore non-evaluable prefixed function literals in a statement."""
    return _RestoreNonEvaluableFunctionsTransformer(context).rewrite(statement)
