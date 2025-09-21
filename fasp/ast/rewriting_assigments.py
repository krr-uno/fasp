from dataclasses import dataclass
from functools import singledispatchmethod
from typing import Any, Iterable

from clingo import ast
from clingo.core import Library

from fasp.ast import AssignmentRule, HeadSimpleAssignment
from fasp.ast.protecting import COMPARISON_NAME
from fasp.util.ast import AST, SyntacticError, function_arguments, is_function

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


class AssignmentTransformer:
    """
    Visitor to collect function symbols from the AST.
    """

    def __init__(self, prefix: str = "F", rewrite: bool = True) -> None:
        self.prefix = prefix
        self.rewrite = rewrite
        self.errors: list[SyntacticError] = []
        self.function_symbols: set[SymbolSignature] = set()
        self._UNCOLLECTABLE = {  # pylint: disable=invalid-name
            ast.LiteralBoolean,
            ast.LiteralSymbolic,
            ast.HeadTheoryAtom,
            ast.StatementTheory,
            ast.StatementOptimize,
            ast.StatementWeakConstraint,
            ast.StatementShow,
            ast.StatementShowNothing,
            ast.StatementShowSignature,
            ast.StatementProject,
            ast.StatementProjectSignature,
            ast.StatementDefined,
            ast.StatementExternal,
            ast.StatementEdge,
            ast.StatementHeuristic,
            ast.StatementScript,
            ast.StatementInclude,
            ast.StatementProgram,
            ast.StatementParts,
            ast.StatementConst,
            ast.StatementComment,
        }

    @singledispatchmethod
    def collect(self, node: Any, *args: Any, **kwargs: Any) -> None:
        """
        Visit the given AST node and check for invalid AST types.

        Parameters
        ----------
        node : AST
            The AST node to visit.
        """
        if type(node) not in self._UNCOLLECTABLE:
            node.visit(self.collect, args, kwargs)

    @collect.register
    def _(
        self, node: ast.StatementRule | AssignmentRule, *args: Any, **kwargs: Any
    ) -> None:
        """
        Visit a Rule node and collect function symbols from the head.
        """
        node.head.visit(self.collect, *args, **kwargs)

    @collect.register
    def _(self, node: HeadSimpleAssignment, *_args: Any, **_kwargs: Any) -> None:
        """
        Visit a Comparison node (assignment) and record the function name and arity.
        """
        name, arguments = function_arguments(node.assigned_function)
        self.function_symbols.add(SymbolSignature(name, len(arguments)))


class ParsingException(Exception):
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


def get_evaluable_functions(program: Iterable[AST]) -> set[SymbolSignature]:
    """
    Collects all evaluable function symbols from the given program.

    Args:
        program (Iterable[AST]): The program to analyze.

    Returns:
        set[FunctionSymbol]: A set of FunctionSymbol instances representing the functions found.
    """
    collector = AssignmentTransformer()
    for statement in program:
        collector.collect(statement)
    if collector.errors:
        raise ParsingException(collector.errors)
    return collector.function_symbols
