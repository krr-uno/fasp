"""
Generic AST collection helpers that do not depend on ``funasp.ast``.
"""

from clingo_funasp import ast, symbol

from funasp.util.ast import AST
from funasp.util.types import SymbolSignature


def collect_predicates(node: AST) -> set[SymbolSignature]:
    """Collect the signatures of all predicate atoms occurring in ``node``."""
    collector = _PredicateCollector()
    return collector.collect(node)


class _PredicateCollector:
    """
    Class to collect predicate signatures from an AST subtree.

    Usage:
        collector = _PredicateCollector()
        predicates = collector.collect(node)
    """

    def __init__(self) -> None:
        """Initialize the collector state for a new predicate traversal."""
        self.predicates: set[SymbolSignature] = set()

    def collect(self, node: AST) -> set[SymbolSignature]:
        """Collect predicate signatures from the given AST node."""
        self._collect_predicates(node)
        return self.predicates

    def _collect_predicates(self, node: AST) -> None:
        """Recursively collect predicate signatures from the given subtree."""
        if isinstance(node, ast.LiteralSymbolic):
            self.predicates |= _atom_signatures(node.atom)
            return
        node.visit(self._collect_predicates)


def _atom_signatures(atom: AST) -> set[SymbolSignature]:
    """Return the predicate signatures declared by a symbolic atom."""
    if isinstance(atom, ast.TermFunction):
        return {
            SymbolSignature(atom.name, len(arguments.arguments))
            for arguments in atom.pool
        }
    if (
        isinstance(atom, ast.TermSymbolic)
        and atom.symbol.type == symbol.SymbolType.Function
    ):
        return {SymbolSignature(atom.symbol.name, len(atom.symbol.arguments))}
    return set()


def collect_variables(node: AST) -> set[str]:
    """Collect all variable names that occur in the given AST node."""
    collector = _VariableCollector()
    return collector.collect(node)


def collect_variables_ordered(node: AST) -> list[str]:
    """Collect distinct variable names in order of first occurrence."""
    collector = _OrderedVariableCollector()
    return collector.collect(node)


class _OrderedVariableCollector:
    """
    Class to collect distinct variables preserving first-occurrence order.

    Usage:
        collector = _OrderedVariableCollector()
        used_vars = collector.collect(node)
    """

    def __init__(self) -> None:
        """Initialize the collector state for a new variable traversal."""
        self.used: dict[str, None] = {}

    def collect(self, node: AST) -> list[str]:
        """Collect ordered distinct variable names from the given AST node."""
        self._collect_vars(node)
        return list(self.used)

    def _collect_vars(self, node: AST) -> None:
        """Recursively collect variables from the given AST subtree."""
        if isinstance(node, ast.TermVariable):
            self.used[node.name] = None
            return
        if isinstance(node, ast.TermSymbolic):
            return
        node.visit(self._collect_vars)


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
