from dataclasses import dataclass
from functools import singledispatch
from typing import Any, Iterable

from clingo import ast

from fasp.ast._nodes import FASP_AST, AssignmentRule, HeadSimpleAssignment
from fasp.util.ast import function_arguments


def collect_variables(node: FASP_AST) -> set[str]:
    collector = _VariableCollector()
    return collector.collect(node)


@dataclass(frozen=True, slots=True, order=True)
class SymbolSignature:
    """
    Represents a function symbol with its name and arity.

    Attributes:
        name (str): The name of the function.
        arity (int): The number of arguments the function takes.
    """

    name: str
    arity: int

    def __str__(self) -> str:
        return f"{self.name}/{self.arity}"


def collect_evaluable_functions(program: Iterable[FASP_AST]) -> set[SymbolSignature]:
    """
    Collects all evaluable function symbols from the given program.

    Args:
        program (Iterable[AST]): The program to analyze.

    Returns:
        set[FunctionSymbol]: A set of FunctionSymbol instances representing the functions found.
    """
    get_evaluable_functions = set()
    for statement in program:
        if isinstance(statement, AssignmentRule):
            get_evaluable_functions |= _get_evaluable_functions_head(statement.head)
    return get_evaluable_functions


@singledispatch
def _get_evaluable_functions_head(_: Any) -> set[SymbolSignature]:
    assert False, f"Unsupported type: {_.__class__}"  # pragma: no cover


@_get_evaluable_functions_head.register
def _(head: HeadSimpleAssignment) -> set[SymbolSignature]:
    name, arguments = function_arguments(head.assigned_function)
    return {SymbolSignature(name, len(arguments))}


class _VariableCollector:
    """
    Class to collect variables from a list of AST statements.

    Usage:
        collector = VariableCollector()
        used_vars = collector.collect(statements)
    """

    def __init__(self) -> None:
        self.used: set[str] = set()

    def collect(self, node: FASP_AST) -> set[str]:
        self._collect_vars(node)
        return self.used

    def _collect_vars(self, node: FASP_AST) -> None:
        if isinstance(node, ast.TermVariable):
            self.used.add(node.name)
            return
        if isinstance(node, ast.TermSymbolic):
            return
        node.visit(self._collect_vars)
