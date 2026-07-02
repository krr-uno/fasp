"""
Generic AST collection helpers that do not depend on ``funasp.ast``.
"""

from clingo_funasp import ast

from funasp.util.ast import AST


def collect_variables(node: AST) -> set[str]:
    """Collect all variable names that occur in the given AST node."""
    collector = _VariableCollector()
    return collector.collect(node)


class _VariableCollector:
    """
    Class to collect variables from a list of AST statements.

    Usage:
        collector = VariableCollector()
        used_vars = collector.collect(statements)
    """

    def __init__(self) -> None:
        """Initialize the collector state for a new variable traversal."""
        self.used: set[str] = set()

    def collect(self, node: AST) -> set[str]:
        """Collect variable names from the given AST node."""
        self._collect_vars(node)
        return self.used

    def _collect_vars(self, node: AST) -> None:
        """Recursively collect variables from the given AST subtree."""
        if isinstance(node, ast.TermVariable):
            self.used.add(node.name)
            return
        if isinstance(node, ast.TermSymbolic):
            return
        node.visit(self._collect_vars)
