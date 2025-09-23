from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any, Self

from clingo import ast
from clingo.core import Library, Location

from fasp import util
from fasp.util import ast as util_ast


class _AssignmentAST:
    pass


@dataclass
class HeadSimpleAssignment(_AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    value: util_ast.TermAST

    def __str__(self):
        return f"{str(self.assigned_function)} := {str(self.value)}"


_AGGREGATE_FUNCTION_TO_STR = {
    ast.AggregateFunction.Sum: "#sum",
    ast.AggregateFunction.Count: "#count",
    ast.AggregateFunction.Min: "#min",
    ast.AggregateFunction.Max: "#max",
}


@dataclass
class HeadAggregateAssignment(_AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    aggregate_function: ast.AggregateFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self):
        return f"{str(self.assigned_function)} := {_AGGREGATE_FUNCTION_TO_STR[self.aggregate_function]}{{{'; '.join(map(str, self.elements))}}}"


@dataclass
class HeadChoiceAssignment(_AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self):
        return (
            f"{str(self.assigned_function)} in {{{'; '.join(map(str, self.elements))}}}"
        )


HeadAssignment = HeadSimpleAssignment | HeadAggregateAssignment


@dataclass
class AssignmentRule(_AssignmentAST):
    library: Library
    location: Location
    head: HeadAssignment
    body: Sequence[util_ast.LiteralAST]

    def __str__(self):
        if not self.body:
            return f"{str(self.head)}."
        body = "; ".join(map(str, self.body))
        return f"{str(self.head)} :- {body}."


StatementAST = util_ast.StatementAST | AssignmentRule
AST = util_ast.AST | AssignmentRule | HeadAssignment
