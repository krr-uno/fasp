from abc import abstractmethod
from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any, Self, TypeVar

from clingo import ast
from clingo.core import Library, Location

from fasp.util import ast as util_ast

from clingo.ast import (
    ArgumentTuple,
    BodyAggregate,
    BodyAggregateElement,
    BodyConditionalLiteral,
    BodySetAggregate,
    BodySimpleLiteral,
    BodyTheoryAtom,
    Edge,
    HeadAggregate,
    HeadAggregateElement,
    HeadConditionalLiteral,
    HeadDisjunction,
    HeadSetAggregate,
    HeadSimpleLiteral,
    HeadTheoryAtom,
    LeftGuard,
    LiteralBoolean,
    LiteralComparison,
    LiteralSymbolic,
    OptimizeElement,
    OptimizeTuple,
    ProgramPart,
    Projection,
    RightGuard,
    SetAggregateElement,
    StatementComment,
    StatementConst,
    StatementDefined,
    StatementEdge,
    StatementExternal,
    StatementHeuristic,
    StatementInclude,
    StatementOptimize,
    StatementParts,
    StatementProgram,
    StatementProject,
    StatementProjectSignature,
    StatementRule,
    StatementScript,
    StatementShow,
    StatementShowNothing,
    StatementShowSignature,
    StatementTheory,
    StatementWeakConstraint,
    TermAbsolute,
    TermBinaryOperation,
    TermFormatString,
    TermFunction,
    TermSymbolic,
    TermTuple,
    TermUnaryOperation,
    TermVariable,
    TheoryAtomDefinition,
    TheoryAtomElement,
    TheoryGuardDefinition,
    TheoryOperatorDefinition,
    TheoryRightGuard,
    TheoryTermDefinition,
    TheoryTermFunction,
    TheoryTermSymbolic,
    TheoryTermTuple,
    TheoryTermUnparsed,
    TheoryTermVariable,
    UnparsedElement,
)


class _AssignmentAST:

    @abstractmethod
    def to_dict(self) -> dict[str, Any]: ...

    def update(self, library: Library, **kwargs: Any) -> Self:
        d = self.to_dict()
        d.update(kwargs)
        d["library"] = library
        return self.__class__(**d)


@dataclass
class HeadSimpleAssignment(_AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    value: util_ast.TermAST

    def __str__(self) -> str:
        return f"{str(self.assigned_function)} := {str(self.value)}"

    def to_dict(self) -> dict[str,Any]: # pragma: no cover
        return {
            "location": self.location,
            "assigned_function": self.assigned_function,
            "value": self.value,
        }

    def transform( # pragma: no cover
        self, library: Library, transformer: Any, *args: Any, **kwargs: Any
    ) -> Self:
        new_assigned_function = transformer(self.assigned_function, *args, **kwargs)
        new_value = transformer(self.value, *args, **kwargs)
        if not new_assigned_function and not new_value:
            return self
        new_assigned_function = new_assigned_function or self.assigned_function
        new_value = new_value or self.value
        return self.__class__(
            library, self.location, new_assigned_function, new_value
        )


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

    def __str__(self) -> str: # pragma: no cover
        return f"{str(self.assigned_function)} := {_AGGREGATE_FUNCTION_TO_STR[self.aggregate_function]}{{{'; '.join(map(str, self.elements))}}}"

    def to_dict(self) -> dict[str,Any]:
        return {
            "location": self.location,
            "assigned_function": self.assigned_function,
            "aggregate_function": self.aggregate_function,
            "elements": self.elements,
        }

    def visit(self, visitor: Any, *args: Any, **kwargs: Any) -> None:
        visitor(self.assigned_function, *args, **kwargs)
        for elem in self.elements:
            visitor(elem, *args, **kwargs)

    def transform( # pragma: no cover
        self, library: Library, transformer: Any, *args: Any, **kwargs: Any
    ) -> Self:
        raise NotImplementedError()


@dataclass
class HeadChoiceAssignment(_AssignmentAST):
    library: Library
    location: Location
    assigned_function: ast.TermFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self) -> str: # pragma: no cover
        return (
            f"{str(self.assigned_function)} in {{{'; '.join(map(str, self.elements))}}}"
        )

    def to_dict(self) -> dict[str,Any]: # pragma: no cover
        return {
            "location": self.location,
            "assigned_function": self.assigned_function,
            "elements": self.elements,
        }


HeadAssignment = HeadSimpleAssignment | HeadAggregateAssignment


@dataclass
class AssignmentRule(_AssignmentAST):
    library: Library
    location: Location
    head: HeadAssignment
    body: Sequence[util_ast.LiteralAST]

    def __str__(self) -> str:
        if not self.body:
            return f"{str(self.head)}."
        body = "; ".join(map(str, self.body))
        return f"{str(self.head)} :- {body}."

    def to_dict(self) -> dict[str,Any]:
        return {
            "location": self.location,
            "head": self.head.to_dict(),
            "body": self.body,
        }

    def visit(self, visitor: Any, *args: Any, **kwargs: Any) -> None:
        visitor(self.head, *args, **kwargs)
        for lit in self.body:
            visitor(lit, *args, **kwargs)

    def transform(self, library: Library, transformer: Any, *args: Any, **kwargs: Any) -> Self: # pragma: no cover
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
        return self.__class__(library, self.location, new_head, new_body)


FASP_Statement = util_ast.StatementAST | AssignmentRule
FASP_AST = util_ast.AST | AssignmentRule | HeadAssignment

FASP_AST_T = TypeVar(
    "FASP_AST_T",
    AssignmentRule,
    HeadSimpleAssignment,
    HeadAggregateAssignment,
    ArgumentTuple,
    BodyAggregate,
    BodyAggregateElement,
    BodyConditionalLiteral,
    BodySetAggregate,
    BodySimpleLiteral,
    BodyTheoryAtom,
    Edge,
    HeadAggregate,
    HeadAggregateElement,
    HeadConditionalLiteral,
    HeadDisjunction,
    HeadSetAggregate,
    HeadSimpleLiteral,
    HeadTheoryAtom,
    LeftGuard,
    LiteralBoolean,
    LiteralComparison,
    LiteralSymbolic,
    OptimizeElement,
    OptimizeTuple,
    ProgramPart,
    Projection,
    RightGuard,
    SetAggregateElement,
    StatementComment,
    StatementConst,
    StatementDefined,
    StatementEdge,
    StatementExternal,
    StatementHeuristic,
    StatementInclude,
    StatementOptimize,
    StatementParts,
    StatementProgram,
    StatementProject,
    StatementProjectSignature,
    StatementRule,
    StatementScript,
    StatementShow,
    StatementShowNothing,
    StatementShowSignature,
    StatementTheory,
    StatementWeakConstraint,
    TermAbsolute,
    TermBinaryOperation,
    TermFunction,
    TermSymbolic,
    TermTuple,
    TermUnaryOperation,
    TermVariable,
    TheoryAtomDefinition,
    TheoryAtomElement,
    TheoryGuardDefinition,
    TheoryOperatorDefinition,
    TheoryRightGuard,
    TheoryTermDefinition,
    TheoryTermFunction,
    TheoryTermSymbolic,
    TheoryTermTuple,
    TheoryTermUnparsed,
    TheoryTermVariable,
    UnparsedElement,
)