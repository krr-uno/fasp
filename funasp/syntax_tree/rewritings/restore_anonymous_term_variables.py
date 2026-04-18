from functools import singledispatchmethod
from typing import Any, Iterable

from clingo import ast

from funasp.syntax_tree._context import RewriteContext
from funasp.util.ast import AST

class _RestoreAnonymousTermVariablesTransformer:
    """Normalize anonymous term variables so their name is always "_"."""

    def __init__(self, context: RewriteContext) -> None:
        """Initialize the transformer with the rewrite context."""
        self.library = context.lib.library

    @singledispatchmethod
    def dispatch(self, node: AST) -> AST | None:  # pragma: no cover
        """Recursively dispatch across AST nodes using clingo's transform API."""
        return node.transform(self.library, self.dispatch)

    @dispatch.register
    def _(self, node: ast.TermVariable) -> ast.TermVariable | None:
        """Restore anonymous term-variable names to "_"."""
        if not node.anonymous:
            return None

        if node.name == "_":
            return None

        return ast.TermVariable(self.library, node.location, "_", anonymous=True)

    def rewrite(self, node: AST) -> AST:
        """Apply anonymous-term-variable restoration to one AST node."""
        rewritten = self.dispatch(node)
        if rewritten is None:
            return node
        assert isinstance(rewritten, AST)
        return rewritten


def restore_anonymous_term_variables(
    context: RewriteContext,
    statement: ast.Statement,
) -> ast.Statement:
    """Restore anonymous term-variable names in a statement."""
    rewritten = _RestoreAnonymousTermVariablesTransformer(context).rewrite(statement)
    assert isinstance(rewritten, ast.Statement)
    return rewritten


def restore_anonymous_term_variables_list(
    context: RewriteContext,
    statements: Iterable[ast.Statement],
) -> list[ast.Statement]:
    """Restore anonymous term-variable names in a statement sequence."""
    return [
        restore_anonymous_term_variables(context, statement) for statement in statements
    ]
