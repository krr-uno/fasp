from filecmp import cmp
import re
from tkinter import N
from typing import Iterable, NamedTuple

from dataclasses import dataclass
from fasp.util.ast import SyntacticCheckVisitor, SyntacticError

from clingo import ast
from clingo.ast import ASTType

INVALID_ASTTYPES = {
    # ASTType.Aggregate,
    # ASTType.ConditionalLiteral,
    ASTType.Defined,
    ASTType.Disjunction,
    ASTType.Edge,
    ASTType.HeadAggregate,
    ASTType.HeadAggregateElement,
    ASTType.Heuristic,
    ASTType.Id,
    ASTType.Minimize,
    ASTType.ProjectAtom,
    ASTType.ProjectSignature,
    ASTType.Script,
    ASTType.TheoryAtom,
    ASTType.TheoryAtomDefinition,
    ASTType.TheoryAtomElement,
    ASTType.TheoryDefinition,
    ASTType.TheoryFunction,
    ASTType.TheoryGuard,
    ASTType.TheoryGuardDefinition,
    ASTType.TheoryOperatorDefinition,
    ASTType.TheorySequence,
    ASTType.TheoryTermDefinition,
    ASTType.TheoryUnparsedTerm,
    ASTType.TheoryUnparsedTermElement,
}


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


class EvaluableFunctionCollector(SyntacticCheckVisitor):
    """
    Visitor to collect function symbols from the AST.
    """

    def __init__(self):
        super().__init__(INVALID_ASTTYPES)
        self.function_symbols = set()

    def visit_Rule(self, node, *args, **kwargs):
        """Visit a Rule node and process its head."""
        self.visit(node.head, *args, **kwargs)
        # only visits the head
        return node

    def visit_Comparison(self, node, *args, **kwargs):
        """Visit a Comparison node (assignment) and record the function name and arity."""
        if kwargs["sign"] != ast.Sign.NoSign:
            self.errors.append(
                SyntacticError(
                    kwargs["location"],
                    f"unexpected negated comparison {node} in the head. Assignments are of the form 'FUNCTION = TERM'.",
                    node.ast_type,
                )
            )
            return node
        if (
            node.term.ast_type != ASTType.Function
            or len(node.guards) != 1
            or node.guards[0].comparison != ast.ComparisonOperator.Equal
        ):
            self.errors.append(
                SyntacticError(
                    kwargs["location"],
                    f"unexpected comparison {node} in the head. Assignments are of the form 'FUNCTION = TERM'.",
                    node.ast_type,
                )
            )
            return node
        self.function_symbols.add(
            SymbolSignature(node.term.name, len(node.term.arguments))
        )
        return node


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


def get_evaluable_functions(program: Iterable[ast.AST]) -> set[SymbolSignature]:
    """
    Collects all evaluable function symbols from the given program.

    Args:
        program (Iterable[ast.AST]): The program to analyze.

    Returns:
        set[FunctionSymbol]: A set of FunctionSymbol instances representing the functions found.
    """
    collector = EvaluableFunctionCollector()
    for statement in program:
        collector.visit(statement)
    if collector.errors:
        raise ParsingException(collector.errors)
    return collector.function_symbols
