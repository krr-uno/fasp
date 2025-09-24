from dataclasses import dataclass
from functools import singledispatch
from typing import Any, Iterable

from fasp.ast import FASP_AST, AssignmentRule, HeadSimpleAssignment
from fasp.util.ast import SyntacticError, function_arguments

# INVALID_ASTTYPES = {}


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


class ParsingException(Exception):  # pragma: no cover
    """
    Exception raised for errors in the parsing process.

    Attributes:
        message (str): The error message.
    """

    def __init__(self, errors: list[SyntacticError]):
        """
        Initialize the ParsingException with a list of syntactic errors.
        """
        super().__init__(errors)
        self.errors = errors

    def __str__(self) -> str:
        return f"ParsingException: {self.errors}"  # pragma: no cover


@singledispatch
def _get_evaluable_functions_head(_: Any) -> set[SymbolSignature]:
    assert False, f"Unsupported type: {_.__class__}"  # pragma: no cover


@_get_evaluable_functions_head.register
def _(head: HeadSimpleAssignment) -> set[SymbolSignature]:
    name, arguments = function_arguments(head.assigned_function)
    return {SymbolSignature(name, len(arguments))}


def get_evaluable_functions(program: Iterable[FASP_AST]) -> set[SymbolSignature]:
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
