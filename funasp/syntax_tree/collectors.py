from functools import singledispatch
from typing import Any, Iterable

from clingo import ast

from funasp.syntax_tree._nodes import (
    FASP_AST,
    AssignmentRule,
    HeadAggregateAssignment,
    HeadAggregateAssignmentElement,
    HeadSimpleAssignment,
)
from funasp.syntax_tree.types import SymbolSignature
from funasp.util.ast import function_arguments


def collect_variables(node: FASP_AST) -> set[str]:
    collector = _VariableCollector()
    return collector.collect(node)


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
# TODO: Need to make it a visitor
def _get_evaluable_functions_head(node: Any) -> set[SymbolSignature]:
    assert False, f"Unsupported type: {_.__class__}"  # pragma: no cover


@_get_evaluable_functions_head.register
def _(head: HeadSimpleAssignment) -> set[SymbolSignature]:
    name, arguments = function_arguments(head.assigned_function)
    return {SymbolSignature(name, len(arguments))}


@_get_evaluable_functions_head.register
def _(head: HeadAggregateAssignment) -> set[SymbolSignature]:
    evaluable_functions = set()
    for element in head.elements:
        if isinstance(element, HeadAggregateAssignmentElement):
            name, arguments = function_arguments(element.assignment.assigned_function)
            evaluable_functions.add(SymbolSignature(name, len(arguments)))
    return evaluable_functions


# NOTE: Is this correct?
# @_get_evaluable_functions_head.register
# def _(head: ChoiceAssignment) -> set[SymbolSignature]:
#     evaluable_functions = set()
#     for element in head.elements:
#         if isinstance(element, AssignmentAggregateElement):
#             name, arguments = function_arguments(element.assignment.assigned_function)
#             evaluable_functions.add(SymbolSignature(name, len(arguments)))
#     return evaluable_functions


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
