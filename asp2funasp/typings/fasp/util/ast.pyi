from collections.abc import Callable, Iterable, Sequence
from typing import TypeAlias, TypeVar
from typing_extensions import TypeIs

from clingo import ast
from clingo.core import Library
from clingo.symbol import Symbol

StatementAST: TypeAlias = ast.Statement
TermAST: TypeAlias = ast.Term
ArgumentAST: TypeAlias = ast.TermOrProjection
LiteralAST: TypeAlias = ast.Literal
BodyLiteralAST: TypeAlias = ast.BodyLiteral
HeadLiteralAST: TypeAlias = ast.HeadLiteral

AST: TypeAlias = (
    StatementAST
    | TermAST
    | LiteralAST
    | ast.ArgumentTuple
    | BodyLiteralAST
    | ast.BodyAggregateElement
    | ast.Edge
    | ast.HeadAggregateElement
    | HeadLiteralAST
    | ast.LeftGuard
    | ast.OptimizeElement
    | ast.OptimizeTuple
    | ast.ProgramPart
    | ast.Projection
    | ast.RightGuard
    | ast.SetAggregateElement
    | ast.TheoryAtomDefinition
    | ast.TheoryAtomElement
    | ast.TheoryGuardDefinition
    | ast.TheoryOperatorDefinition
    | ast.TheoryRightGuard
    | ast.TheoryTermDefinition
    | ast.TheoryTermFunction
    | ast.TheoryTermSymbolic
    | ast.TheoryTermTuple
    | ast.TheoryTermUnparsed
    | ast.TheoryTermVariable
    | ast.UnparsedElement
)

FunctionLikeAST: TypeAlias = ast.TermFunction | ast.TermSymbolic | ast.TermTuple | Symbol

T = TypeVar("T")
R = TypeVar("R")

def is_function(node: AST) -> TypeIs[ast.TermFunction | ast.TermSymbolic]: ...
def function_arguments(
    node: FunctionLikeAST,
) -> tuple[str, Sequence[ast.TermOrProjection] | Sequence[Symbol]]: ...
def function_arguments_ast(
    library: Library,
    node: ast.TermFunction | ast.TermSymbolic,
) -> tuple[str, Sequence[ast.Term]]: ...
def transform_iterable(
    library: Library,
    iterable: Iterable[T],
    fun: Callable[[Library, T], R | None],
) -> list[T | R] | None: ...
