from functools import singledispatch
from typing import Any, Iterable

from clingo import ast, symbol

from funasp.syntax_tree._nodes import (
    FASP_AST,
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    HeadAggregateAssignment,
    HeadAggregateAssignmentElement,
    HeadSimpleAssignment,
)
from funasp.syntax_tree.types import SymbolSignature


def collect_variables(node: FASP_AST) -> set[str]:
    """Collect all variable names that occur in the given AST node."""
    collector = _VariableCollector()
    return collector.collect(node)


def collect_evaluable_functions(
    program: Iterable[FASP_AST],
) -> set[SymbolSignature]:
    """
    Collects all evaluable function symbols from the given program.

    Args:
        program (Iterable[AST]): The program to analyze.

    Returns:
        set[FunctionSymbol]: A set of FunctionSymbol instances representing the functions found.
    """
    # if isinstance(program, FASP_AST):
    #     if isinstance(program, AssignmentRule):
    #         return _get_evaluable_functions_head(program.head)
    #     return set()
    get_evaluable_functions = set()
    for statement in program:
        if isinstance(statement, AssignmentRule):
            get_evaluable_functions |= _get_evaluable_functions_head(statement.head)
    return get_evaluable_functions


@singledispatch
# TODO: Need to make it a visitor
def _get_evaluable_functions_head(node: Any) -> set[SymbolSignature]:
    """Collect evaluable function signatures from a supported assignment head."""
    assert False, f"Unsupported type: {_.__class__}"  # pragma: no cover


@_get_evaluable_functions_head.register
def _(head: HeadSimpleAssignment) -> set[SymbolSignature]:
    """Collect the signature of the function assigned by a simple head assignment."""
    assigned_function = head.assigned_function
    if isinstance(assigned_function, ast.TermFunction):
        name = assigned_function.name
        return {
            SymbolSignature(name, len(pool.arguments))
            for pool in assigned_function.pool
        }
    elif (
        isinstance(assigned_function, ast.TermSymbolic)
        and assigned_function.symbol.type == symbol.SymbolType.Function
    ):
        return {
            SymbolSignature(
                assigned_function.symbol.name, len(assigned_function.symbol.arguments)
            )
        }
    assert (
        False
    ), f"Unsupported assigned function type: {assigned_function.__class__}"  # pragma: no cover


@_get_evaluable_functions_head.register
def _(head: HeadAggregateAssignment | ChoiceAssignment) -> set[SymbolSignature]:
    """Collect evaluable function signatures from assignment-bearing aggregate elements."""
    evaluable_functions = set()
    for element in head.elements:
        if isinstance(
            element, HeadAggregateAssignmentElement | AssignmentAggregateElement
        ):
            evaluable_functions |= _get_evaluable_functions_head(element.assignment)
    return evaluable_functions


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

    def collect(self, node: FASP_AST) -> set[str]:
        """Collect variable names from the given AST node."""
        self._collect_vars(node)
        return self.used

    def _collect_vars(self, node: FASP_AST) -> None:
        """Recursively collect variables from the given AST subtree."""
        if isinstance(node, ast.TermVariable):
            self.used.add(node.name)
            return
        if isinstance(node, ast.TermSymbolic):
            return
        node.visit(self._collect_vars)
