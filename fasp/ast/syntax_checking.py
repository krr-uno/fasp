from ast import arguments
from functools import singledispatchmethod
from typing import Any, Iterable
from dataclasses import dataclass

from clingo import ast

from fasp.util.ast import SyntacticError, AST, is_function, function_arguments


INVALID_ASTTYPES = {}


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

    def __str__(self):
        return f"{self.name}/{self.arity}"


class EvaluableFunctionCollector:
    """
    Visitor to collect function symbols from the AST.
    """

    def __init__(self):
        self.errors = []
        self.function_symbols = set()
        self._UNCOLLECTABLE = {ast.LiteralBoolean, ast.LiteralSymbolic, ast.HeadTheoryAtom, 
                      ast.StatementTheory, ast.StatementOptimize, ast.StatementWeakConstraint, ast.StatementShow, ast.StatementShowNothing, ast.StatementShowSignature, ast.StatementProject, ast.StatementProjectSignature, ast.StatementDefined, ast.StatementExternal, ast.StatementEdge, ast.StatementHeuristic, ast.StatementScript, ast.StatementInclude, ast.StatementProgram, ast.StatementParts, ast.StatementConst, ast.StatementComment}

    @singledispatchmethod
    def collect(self, node, *args: Any, **kwargs: Any) -> None:
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
    def _(self, node: ast.StatementRule, *args, **kwargs) -> None:
        """
        Visit a Rule node and collect function symbols from the head.
        """
        return node.head.visit(self.collect, *args, **kwargs)

    @collect.register
    def _(self, node: ast.LiteralComparison, *args, **kwargs) -> None:
        """
        Visit a Comparison node (assignment) and record the function name and arity.
        """
        if node.sign != ast.Sign.NoSign:
            self.errors.append(
                SyntacticError(
                    node.location,
                    f"unexpected negated comparison {node} in the head. Assignments are of the form 'FUNCTION = TERM'.",
                    type(node),
                )
            )
            return node
        if (
            not is_function(node.left)
            or len(node.right) != 1
            or node.right[0].relation != ast.Relation.Equal
        ):
            self.errors.append(
                SyntacticError(
                    node.location,
                    f"unexpected comparison {node} in the head. Assignments are of the form 'FUNCTION = TERM'.",
                    type(node),
                )
            )
            return node
        name, arguments = function_arguments(node.left)
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

    def __str__(self):
        return f"ParsingException: {self.errors}"


def get_evaluable_functions(program: Iterable[AST]) -> set[SymbolSignature]:
    """
    Collects all evaluable function symbols from the given program.

    Args:
        program (Iterable[AST]): The program to analyze.

    Returns:
        set[FunctionSymbol]: A set of FunctionSymbol instances representing the functions found.
    """
    collector = EvaluableFunctionCollector()
    for statement in program:
        if isinstance(statement, ast.StatementRule):
            collector.collect(statement)
    if collector.errors:
        raise ParsingException(collector.errors)
    return collector.function_symbols
