from abc import abstractmethod
from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any, Self, TypeVar

from clingo import ast
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
from clingo.core import Library, Location

from fasp.util import ast as util_ast


class AssignmentAST:
    """
    Abstract base class for all assignment-related (:=) AST nodes in FASP.

    This class defines the common interface and traversal utilities:
    - to_dict() must be implemented by all subclasses to return
      a serializable mapping of their fields.
    - update(**kwargs)
    - visit(visitor, *args, **kwargs)
    - transform(library, transformer, *args, **kwargs)

    Notes
    -----
    - These methods are implemented to match the interface of clingo.ast nodes.
    """

    @abstractmethod
    def to_dict(self) -> dict[str, Any]:
        """
        Return a serializable dictionary representation of this node.

        Notes
        -----
        Subclasses must ensure that the keys returned match their dataclass
        constructor parameters. Typically includes a "type" field for
        debugging or serialization.
        """

    def update(self, lib: Library | None = None, **kwargs: Any) -> Self:
        """
        Create a new instance with some fields replaced.

        Parameters
        ----------
        **kwargs : Any
            Field overrides to apply.

        Returns
        -------
        Self
            A new instance of the same class with updated fields.
        """
        d = self.to_dict()
        d.update(kwargs)
        d.pop("type", None)
        return self.__class__(**d)

    def visit(self, visitor: Any, *args: Any, **kwargs: Any) -> None:
        """
        Traverse child nodes and apply a visitor function.
        Parameters
        ----------
        visitor : callable
            The visitor function to apply to child nodes.
        *args, **kwargs
            Extra arguments passed through to the visitor.

        Notes
        -----
        The visitor is invoked for each child AST node that implements
        visit. Fields like location and type are ignored.
        """
        d = self.to_dict()
        for key, value in d.items():
            if key in {"location", "type"}:
                continue
            if isinstance(value, Sequence) and not isinstance(value, str):
                for item in value:
                    if hasattr(item, "visit"):
                        visitor(item, *args, **kwargs)
            elif hasattr(value, "visit"):
                visitor(value, *args, **kwargs)

    def transform(  # pragma: no cover
        self, _: Library, transformer: Any, *args: Any, **kwargs: Any
    ) -> Self:
        """
        Recursively transform child nodes and return a new instance.

        Parameters
        ----------
        library : Library
            A library object passed to the transformer. This is not used in the function, but is passed for compatibility with clingo's transform method signature.
        transformer : callable
            The transformer that needs to be applied on the AST node.
        *args, **kwargs
            Extra arguments passed through to the transformer.

        Returns
        -------
        Self
            A new instance with transformed child nodes.

        Notes
        -----
        If the transformer returns None for a child, that child is left
        unchanged. Otherwise, the child is replaced with the returned node.
        """
        d = self.to_dict()
        for key, value in d.items():
            if key in {"location", "type"}:
                continue
            if isinstance(value, Sequence) and not isinstance(value, str):
                new_values = []
                for item in value:
                    if new_item := transformer(item, *args, **kwargs):
                        new_values.append(new_item)
                d[key] = new_values
            elif new_value := transformer(value, *args, **kwargs):
                d[key] = new_value
        d.pop("type", None)
        return self.__class__(**d)


@dataclass
class HeadSimpleAssignment(AssignmentAST):
    """
    A simple assignment head of the form ``f(args) := value``.

    Parameters
    ----------
    location : Location
        Source code location of this assignment.
    assigned_function : ast.TermFunction
        The function symbol being assigned, e.g. ``f(args)``.
    value : TermAST (Defined in util_ast)
        The right-hand side term of the assignment.
    """

    location: Location
    assigned_function: ast.TermFunction
    value: util_ast.TermAST

    def __str__(self) -> str:
        return f"{str(self.assigned_function)} := {str(self.value)}"

    def to_dict(self) -> dict[str, Any]:  # pragma: no cover
        return {
            "type": "HeadSimpleAssignment",
            "location": self.location,
            "assigned_function": self.assigned_function,
            "value": self.value,
        }

    # def transform( # pragma: no cover
    #     self, library: Library, transformer: Any, *args: Any, **kwargs: Any
    # ) -> Self:
    #     new_assigned_function = transformer(self.assigned_function, *args, **kwargs)
    #     new_value = transformer(self.value, *args, **kwargs)
    #     if not new_assigned_function and not new_value:
    #         return self
    #     new_assigned_function = new_assigned_function or self.assigned_function
    #     new_value = new_value or self.value
    #     return self.__class__(
    #         library, self.location, new_assigned_function, new_value
    #     )


_AGGREGATE_FUNCTION_TO_STR = {
    ast.AggregateFunction.Sum: "#sum",
    ast.AggregateFunction.Count: "#count",
    ast.AggregateFunction.Min: "#min",
    ast.AggregateFunction.Max: "#max",
}


@dataclass
class HeadAssignmentAggregate(AssignmentAST):
    """
    An assignment head of the form ``f(args) := #agg{ ... }``.

    Parameters
    ----------
    location : Location
        Source code location.
    assigned_function : ast.TermFunction
        The function symbol on the left-hand side.
    aggregate_function : ast.AggregateFunction
         The aggregate operator (``#sum``, ``#count``, ``#min``, ``#max``).
    elements : Sequence[ast.BodyAggregateElement]
        Elements of the aggregate body.
    """

    location: Location
    assigned_function: ast.TermFunction
    aggregate_function: ast.AggregateFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self) -> str:  # pragma: no cover
        return f"{str(self.assigned_function)} := {_AGGREGATE_FUNCTION_TO_STR[self.aggregate_function]}{{{'; '.join(map(str, self.elements))}}}"

    def to_dict(self) -> dict[str, Any]:
        return {
            "type": "HeadAssignmentAggregate",
            "location": self.location,
            "assigned_function": self.assigned_function,
            "aggregate_function": self.aggregate_function,
            "elements": self.elements,
        }


@dataclass
class AssignmentAggregateElement(AssignmentAST):
    """
    A single assignment element with an optional condition,
    used inside a choice assignment.

    This node wraps a `HeadSimpleAssignment` and associates it with
    zero or more literals that act as a condition.

    Example:
        { f(X) := 1 : p(X); f(X) := 2 : q(X) }.
    In this example, `f(X) := value : condition` becomes an
    AssignmentAggregateElement.

    Parameters
    ----------
    location : Location
        Source code location.
    assignment : HeadSimpleAssignment
        The assignment part of the element (e.g., `f(X) := 1`).
    condition : Sequence[util_ast.LiteralAST]
        Optional literals serving as conditions (e.g., `p(X)`).
    """

    location: Location
    assignment: HeadSimpleAssignment
    condition: Sequence[util_ast.LiteralAST]

    def __str__(self) -> str:
        if self.condition:
            return f"{str(self.assignment)}: {', '.join(map(str, self.condition))}"
        return f"{str(self.assignment)}"

    def to_dict(self) -> dict[str, Any]:  # pragma: no cover
        return {
            "type": "AssignmentAggregateElement",
            "location": self.location,
            "assignment": self.assignment,
            "condition": self.condition,
        }


@dataclass
class ChoiceAssignment(AssignmentAST):
    """
    A choice-style assignment with optional guards.

    Syntax form:
        [left] { element1; element2; ... } [right]

    Each element can be either:
    - `AssignmentAggregateElement`: a simple assignment with an optional condition,
      e.g., `f(X) := 1 : p(X)`.
    - `ast.SetAggregateElement`: a standard clingo set aggregate element.

    Example:
        1 { f(X) := 1 : p(X); f(X) := 2 : q(X) } 3.
        1 { f(X): p(x), a; f(X): b}.

    Parameters
    ----------
    location : Location
        Source code location.
    elements : Sequence[AssignmentAggregateElement | ast.SetAggregateElement]
        Elements inside the choice braces.
    left : LeftGuard, optional
        An optional left guard.
    right : RightGuard, optional
        An optional right guard.
    """

    location: Location
    left: LeftGuard | None
    elements: Sequence[AssignmentAggregateElement | ast.SetAggregateElement]
    right: RightGuard | None

    def __str__(self) -> str:  # pragma: no cover
        left = str(self.left) if self.left else ""
        right = str(self.right) if self.right else ""
        return f"{left}{{ {'; '.join(map(str, self.elements))} }}{right}"

    # Note: Implementation seemed to be incomplete since this class does not have assigned_function
    # but to_dict referred to it and guard fields were missing.

    # Attempt to fix to_dict implementation
    def to_dict(self) -> dict[str, Any]:  # pragma: no cover
        return {
            "type": "HeadChoiceAssignment",
            "location": self.location,
            # "assigned_function": self.assigned_function,
            "elements": self.elements,
            "left": self.left,
            "right": self.right,
        }


@dataclass
class ChoiceSomeAssignment(AssignmentAST):
    """
    An assignment head of the form ``f(args) := #agg{ ... }``.

    Parameters
    ----------
    location : Location
        Source code location.
    assigned_function : ast.TermFunction
        The function symbol on the left-hand side.
    elements : Sequence[ast.BodyAggregateElement]
        Elements of the aggregate body.
    """

    location: Location
    assigned_function: ast.TermFunction
    elements: Sequence[ast.BodyAggregateElement]

    def __str__(self) -> str:  # pragma: no cover
        return f"{str(self.assigned_function)} := #some{{{'; '.join(map(str, self.elements))}}}"

    def to_dict(self) -> dict[str, Any]:  # pragma: no cover
        return {
            "type": "ChoiceSomeAssignment",
            "location": self.location,
            "assigned_function": self.assigned_function,
            "elements": self.elements,
        }


@dataclass
class HeadAggregateAssignmentElement(AssignmentAST):
    """
    A single assignment element with an optional condition,
    used inside a choice assignment.

    This node wraps a `HeadSimpleAssignment` and associates it with
    zero or more literals that act as a condition.

    Example:
        { f(X) := 1 : p(X); f(X) := 2 : q(X) }.
    In this example, `f(X) := value : condition` becomes an
    AssignmentAggregateElement.

    Parameters
    ----------
    location : Location
        Source code location.
    tuple : Sequence[util_ast.TermAST]
        tuple
    assignment : HeadSimpleAssignment
        The assignment part of the element (e.g., `f(X) := 1`).
    condition : Sequence[util_ast.LiteralAST]
        Optional literals serving as conditions (e.g., `p(X)`).

    """

    location: Location
    tuple: Sequence[util_ast.TermAST]
    assignment: HeadSimpleAssignment
    condition: Sequence[util_ast.LiteralAST]

    def __str__(self) -> str:
        _tuple = ""
        if self.tuple:
            _tuple = ",".join(map(str, self.tuple))
            if _tuple != "":
                _tuple += ": "
        if self.condition:
            return (
                f"{_tuple}{str(self.assignment)}: {', '.join(map(str, self.condition))}"
            )
        return f"{_tuple}{str(self.assignment)}"

    def to_dict(self) -> dict[str, Any]:  # pragma: no cover
        return {
            "type": "HeadAggregateAssignmentElement",
            "location": self.location,
            "tuple": self.tuple,
            "assignment": self.assignment,
            "condition": self.condition,
        }


@dataclass
class HeadAggregateAssignment(AssignmentAST):
    """
    An aggregate with an assignment with optional guards.

    Syntax form:
        [left] #agg{ element1; element2; ... } [right]

    Each element can be either:
    - `HeadAggregateAssignmentElement`,
    - `ast.HeadAggregateElement`: a standard clingo Head aggregate element.

    Example:
        1 #count{ f(X,Y) : f(X) := Y : p(X,Y), not in(X); p(X) : p(X) } 1.

    Parameters
    ----------
    location : Location
        Source code location.
    left : LeftGuard, optional
        An optional left guard.
    function : ast.AggregateFunction
        The aggregate function.
    elements : Sequence[HeadAggregateAssignmentElement | ast.HeadAggregateElement]
        Elements inside the choice braces.
    right : RightGuard, optional
        An optional right guard.
    """

    location: Location
    left: LeftGuard | None
    function: ast.AggregateFunction
    elements: Sequence[HeadAggregateAssignmentElement | ast.HeadAggregateElement]
    right: RightGuard | None

    def __str__(self) -> str:  # pragma: no cover
        left = str(self.left) if self.left else ""
        right = str(self.right) if self.right else ""
        return f"{left}{_AGGREGATE_FUNCTION_TO_STR[self.function]}{{ {'; '.join(map(str, self.elements))} }}{right}"

    def to_dict(self) -> dict[str, Any]:  # pragma: no cover
        return {
            "type": "HeadAggregateAssignment",
            "location": self.location,
            "left": self.left,
            "function": self.function,
            "elements": self.elements,
            "right": self.right,
        }


HeadAssignment = (
    HeadSimpleAssignment
    | HeadAssignmentAggregate
    | HeadAggregateAssignment
    | ChoiceAssignment
    | ChoiceSomeAssignment
)


@dataclass
class AssignmentRule(AssignmentAST):
    """
    A full rule with an assignment in its head. Analogous to
    ast.StatementRule but with an assignment (:=) in the head.

    Syntax forms:
    - `head.` (a fact with no body)
    - `head :- body.` (a rule with a non-empty body)

    Parameters
    ----------
    location : Location
        Source code location.
    head : HeadAssignment
        The assignment head (simple or aggregate).
    body : Sequence[util_ast.BodyLiteralAST]
        The rule body literals.
    """

    location: Location
    head: HeadAssignment
    body: Sequence[util_ast.BodyLiteralAST]

    def __str__(self) -> str:  # pragma: no cover
        if not self.body:
            return f"{str(self.head)}."
        body = "; ".join(map(str, self.body))
        return f"{str(self.head)} :- {body}."

    def to_dict(self) -> dict[str, Any]:
        return {
            "type": "AssignmentRule",
            "location": self.location,
            "head": self.head,
            "body": self.body,
        }

    # def visit(self, visitor: Any, *args: Any, **kwargs: Any) -> None:
    #     visitor(self.head, *args, **kwargs)
    #     for lit in self.body:
    #         visitor(lit, *args, **kwargs)

    # def transform(self, library: Library, transformer: Any, *args: Any, **kwargs: Any) -> Self: # pragma: no cover
    #     new_head = transformer(self.head, *args, **kwargs)
    #     new_body = []
    #     new_lit_in_body = False
    #     for lit in self.body:
    #         new_lit = transformer(lit, *args, **kwargs)
    #         if new_lit is not None:
    #             new_body.append(new_lit)
    #             new_lit_in_body = True
    #         else:
    #             new_body.append(lit)
    #     if not new_head and not new_lit_in_body:
    #         return self
    #     new_head = new_head or self.head
    #     if not new_lit_in_body:
    #         new_body = self.body
    #     return self.__class__(library, self.location, new_head, new_body)


FASP_Statement = util_ast.StatementAST | AssignmentRule
FASP_AST = util_ast.AST | AssignmentAST

FASP_AST_T = TypeVar(
    "FASP_AST_T",
    AssignmentAST,
    HeadSimpleAssignment,
    HeadAssignmentAggregate,
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
    TermFormatString,
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
