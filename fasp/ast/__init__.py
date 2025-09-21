from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from typing import Any, Self
from clingo import ast
from clingo.core import Library, Location
from fasp.util.ast import TermAST, LiteralAST


@dataclass
class HeadSimpleAssignment:
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    value: TermAST

    def __str__(self):
        return f"{str(self.assigned_function)} := {str(self.value)}"
    
    def visit(
        self, visitor: Any, *args, **kwargs
    ) -> ast.StatementRule:
        visitor(self, *args, **kwargs)

    def transform(
        self, library: Library, transformer: Any, *args, **kwargs
    ) -> ast.StatementRule:
        new_assigned_function = transformer(self.assigned_function, *args, **kwargs)
        new_value = transformer(self.value, *args, **kwargs)
        if not new_assigned_function and not new_value:
            return self
        new_assigned_function = new_assigned_function or self.assigned_function
        new_value = new_value or self.value
        return HeadSimpleAssignment(library, self.location, new_assigned_function, new_value)




AGGREGATE_FUNCTION_TO_STR = {
    ast.AggregateFunction.Sum: "#sum",
    ast.AggregateFunction.Count: "#count",
    ast.AggregateFunction.Min: "#min",
    ast.AggregateFunction.Max: "#max",
}


@dataclass
class HeadAggregateAssignment:
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    aggregate_function: ast.AggregateFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self):
        return f"{str(self.assigned_function)} := {AGGREGATE_FUNCTION_TO_STR[self.aggregate_function]}{{{'; '.join(map(str, self.elements))}}}"


@dataclass
class HeadChoiceAssignment:
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
class AssignmentRule:
    library: Library
    location: Location
    head: HeadAssignment
    body: Sequence[LiteralAST]

    def __str__(self):
        if not self.body:
            return f"{str(self.head)}."
        body = "; ".join(map(str, self.body))
        return f"{str(self.head)} :- {body}."

    def visit(
        self, visitor: Any, *args, **kwargs
    ) -> None:
        visitor(self, *args, **kwargs)


    def transform(
        self, library: Library, transformer: Any, *args, **kwargs
    ) -> Self:
        new_head = transformer(self.head, *args, **kwargs)
        new_body = []
        new_lit_in_body = False
        for lit in self.body:
            new_lit = transformer(lit, *args, **kwargs)
            if new_lit is not None:
                new_body.append(new_lit)
                new_lit_in_body = True
            else:
                new_body.append(lit)
        if not new_head and not new_lit_in_body:
            return self
        new_head = new_head or self.head
        if not new_lit_in_body:
            new_body = self.body
        return AssignmentRule(library, self.location, new_head, new_body)
