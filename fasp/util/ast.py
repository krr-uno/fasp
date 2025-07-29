from ast import arg
from functools import singledispatchmethod

from platform import node
from typing import AbstractSet, Any, NamedTuple, Optional, Sequence

from clingo import ast
from clingo.symbol import SymbolType, Symbol
from clingo.core import Library, Location, Position
from clingo.ast import (
    AggregateFunction,
    ArgumentTuple,
    BinaryOperator,
    BodyAggregate,
    BodyAggregateElement,
    BodyConditionalLiteral,
    BodySetAggregate,
    BodySimpleLiteral,
    BodyTheoryAtom,
    CommentType,
    Edge,
    HeadAggregate,
    HeadAggregateElement,
    HeadConditionalLiteral,
    HeadDisjunction,
    HeadSetAggregate,
    HeadSimpleLiteral,
    HeadTheoryAtom,
    IncludeType,
    LeftGuard,
    LiteralBoolean,
    LiteralComparison,
    LiteralSymbolic,
    OptimizeElement,
    OptimizeTuple,
    OptimizeType,
    Precedence,
    Program,
    ProgramPart,
    Projection,
    ProjectionMode,
    Relation,
    RewriteContext,
    RightGuard,
    SetAggregateElement,
    Sign,
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
    TheoryAtomType,
    TheoryGuardDefinition,
    TheoryOperatorDefinition,
    TheoryOperatorType,
    TheoryRightGuard,
    TheoryTermDefinition,
    TheoryTermFunction,
    TheoryTermSymbolic,
    TheoryTermTuple,
    TheoryTermUnparsed,
    TheoryTermVariable,
    TheoryTupleType,
    UnaryOperator,
    UnparsedElement,
)


AST = (
    AggregateFunction
    | ArgumentTuple
    | BinaryOperator
    | BodyAggregate
    | BodyAggregateElement
    | BodyConditionalLiteral
    | BodySetAggregate
    | BodySimpleLiteral
    | BodyTheoryAtom
    | CommentType
    | Edge
    | HeadAggregate
    | HeadAggregateElement
    | HeadConditionalLiteral
    | HeadDisjunction
    | HeadSetAggregate
    | HeadSimpleLiteral
    | HeadTheoryAtom
    | IncludeType
    | LeftGuard
    | LiteralBoolean
    | LiteralComparison
    | LiteralSymbolic
    | OptimizeElement
    | OptimizeTuple
    | OptimizeType
    | Precedence
    | Program
    | ProgramPart
    | Projection
    | ProjectionMode
    | Relation
    | RewriteContext
    | RightGuard
    | SetAggregateElement
    | Sign
    | StatementComment
    | StatementConst
    | StatementDefined
    | StatementEdge
    | StatementExternal
    | StatementHeuristic
    | StatementInclude
    | StatementOptimize
    | StatementParts
    | StatementProgram
    | StatementProject
    | StatementProjectSignature
    | StatementRule
    | StatementScript
    | StatementShow
    | StatementShowNothing
    | StatementShowSignature
    | StatementTheory
    | StatementWeakConstraint
    | TermAbsolute
    | TermBinaryOperation
    | TermFunction
    | TermSymbolic
    | TermTuple
    | TermUnaryOperation
    | TermVariable
    | TheoryAtomDefinition
    | TheoryAtomElement
    | TheoryAtomType
    | TheoryGuardDefinition
    | TheoryOperatorDefinition
    | TheoryOperatorType
    | TheoryRightGuard
    | TheoryTermDefinition
    | TheoryTermFunction
    | TheoryTermSymbolic
    | TheoryTermTuple
    | TheoryTermUnparsed
    | TheoryTermVariable
    | TheoryTupleType
    | UnaryOperator
    | UnparsedElement
)


class SyntacticError(NamedTuple):
    """
    Represents a syntactic error in the AST.

    Attributes:
        location: The location in the source code where the error occurred.
        message (str): The error message.
        information (any): Additional information about the error.
    """

    location: Location
    message: str
    information: Optional[Any] = None

    def __str__(self) -> str:
        return f"{self.location}: error: syntax error, {self.message}"


class HeadBodyVisitor:

    def __init__(self, library: Library):
        self.lib = library

    @singledispatchmethod
    def _dispatch(self, expr: AST) -> None:
        """
        Order bodies of statements.
        """
        return expr.visit(self._lib, self._dispatch)

    @_dispatch.register
    def _(self, node: ast.StatementRule, *args: Any, **kwargs: Any) -> None:
        """
        Visit the Rule node, setting the 'head' flag accordingly for children.

        Parameters
        ----------
        node : AST
            The Rule node.

        Returns
        -------
        AST
            The (potentially transformed) Rule node.
        """
        kwargs["head"] = True
        node.head.visit(self._lib, self._dispatch)
        kwargs["head"] = False
        for element in node.body:
            element.visit(self._lib, self._dispatch)


class SyntacticCheckVisitor:
    """
    A visitor that checks for syntactic errors in the AST.

    This visitor does not perform any transformations but can be used to
    traverse the AST and check for specific syntactic conditions.
    """

    def __init__(self, invalid_ASTTypes: AbstractSet[type[AST]]) -> None:
        """
        Initializes the SyntacticCheckVisitor.

        Args:
            invalid_ASTTypes (set[ASTType]): A set of AST types that are considered invalid.
        """
        self.invalid_ASTTypes = invalid_ASTTypes
        self.errors = []

    def visit(self, node: AST, *args: Any, **kwargs: Any) -> None:
        """
        Visit the given AST node and check for invalid AST types.

        Parameters
        ----------
        node : AST
            The AST node to visit.
        """
        if type(node) in self.invalid_ASTTypes:
            self.errors.append(
                SyntacticError(node.location, f"unexpected {node}", type(node))
            )
        else:
            node.visit(self, args, kwargs)

    def __call__(self, node: AST, *args: Any, **kwargs: Any) -> None:
        self.visit(node, *args, **kwargs)


def create_literal(
    library: Library,
    atom: AST,
    sign: ast.Sign = ast.Sign.NoSign,
    *,
    head: bool = False,
    body: bool = False,
) -> AST:
    if hasattr(atom, "location"):
        location = atom.location
    else:
        position = Position(library, "<aux>", 0, 0)
        location = Location(library, position, position)
    literal = ast.LiteralSymbolic(library, location, sign, atom)
    if head:
        return ast.HeadSimpleLiteral(library, literal)
    elif body:
        return ast.BodySimpleLiteral(library, literal)
    else:
        return literal


def is_function(node: AST) -> bool:
    return (
        isinstance(node, ast.TermFunction)
        or isinstance(node, ast.TermSymbolic)
        and node.symbol.type == SymbolType.Function
    )


def function_arguments(
    node: ast.TermFunction | ast.TermSymbolic,
) -> tuple[str, ArgumentTuple | Sequence[Symbol]]:
    if isinstance(node, ast.TermSymbolic):
        name = node.symbol.name
        arguments = node.symbol.arguments
    else:
        name = node.name
        assert len(node.pool) == 1, f"Terms must be unpooled {node}"
        arguments = node.pool[0].arguments
    return name, arguments
