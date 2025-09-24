from typing import (
    Iterable,
)

from clingo import ast

from fasp.ast import FASP_AST, FASP_Statement


class VariableCollector:
    """
    Class to collect variables from a list of AST statements.

    Usage:
        collector = VariableCollector()
        used_vars = collector.collect(statements)
    """

    def __init__(self) -> None:
        self.used: set[str] = set()

    def collect(self, statements: Iterable[FASP_Statement]) -> set[str]:
        for stmt in statements:
            self._collect_vars(stmt)
        return self.used

    def _collect_vars(self, node: FASP_AST) -> None:
        if isinstance(node, ast.TermVariable):
            self.used.add(node.name)
            return
        node.visit(self._collect_vars)


def collect_variables(statements: Iterable[FASP_Statement]) -> set[str]:
    collector = VariableCollector()
    return collector.collect(statements)
