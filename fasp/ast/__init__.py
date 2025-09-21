from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from typing import Any, Self
from clingo import ast
from clingo.core import Library, Location
from fasp.util.ast import TermAST, LiteralAST


class AssignmentAST:

    def visit(self, visitor: Any, *args, **kwargs) -> None:
        visitor(self, *args, **kwargs)

    def transform(
        self, library: Library, transformer: Any, *args, **kwargs
    ) -> Self:
        return transformer(self, *args, **kwargs) or self

@dataclass
class HeadSimpleAssignment(AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    value: TermAST

    def __str__(self):
        return f"{str(self.assigned_function)} := {str(self.value)}"


AGGREGATE_FUNCTION_TO_STR = {
    ast.AggregateFunction.Sum: "#sum",
    ast.AggregateFunction.Count: "#count",
    ast.AggregateFunction.Min: "#min",
    ast.AggregateFunction.Max: "#max",
}


@dataclass
class HeadAggregateAssignment(AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    aggregate_function: ast.AggregateFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self):
        return f"{str(self.assigned_function)} := {AGGREGATE_FUNCTION_TO_STR[self.aggregate_function]}{{{'; '.join(map(str, self.elements))}}}"


@dataclass
class HeadChoiceAssignment(AssignmentAST):
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
class AssignmentRule(AssignmentAST):
    library: Library
    location: Location
    head: HeadAssignment
    body: Sequence[LiteralAST]

    def __str__(self):
        if not self.body:
            return f"{str(self.head)}."
        body = "; ".join(map(str, self.body))
        return f"{str(self.head)} :- {body}."

