import re
import sys
import typing
from typing import (
    AbstractSet,
    Any,
    NamedTuple,
    Optional,
    Sequence,
    TypeIs,
    TypeVar,
    cast,
)

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
from clingo.core import Library, Location, LogLevel, MessageType, Position
from clingo.symbol import Symbol, SymbolType

StatementAST = (
    StatementRule
    | StatementTheory
    | StatementOptimize
    | StatementWeakConstraint
    | StatementShow
    | StatementShowNothing
    | StatementShowSignature
    | StatementProject
    | StatementProjectSignature
    | StatementDefined
    | StatementExternal
    | StatementEdge
    | StatementHeuristic
    | StatementScript
    | StatementInclude
    | StatementProgram
    | StatementParts
    | StatementConst
    | StatementComment
)

TermAST = (
    TermVariable
    | TermSymbolic
    | TermAbsolute
    | TermUnaryOperation
    | TermBinaryOperation
    | TermTuple
    | TermFunction
    | TermFormatString
)
ArgumentAST = TermAST | Projection
LiteralAST = LiteralBoolean | LiteralComparison | LiteralSymbolic

BodyLiteralAST = (
    BodySimpleLiteral
    | BodyAggregate
    | BodySetAggregate
    | BodyTheoryAtom
    | BodyConditionalLiteral
)

AST = (
    StatementAST
    | TermAST
    | LiteralAST
    | ArgumentTuple
    | BodyLiteralAST
    | BodyAggregateElement
    | Edge
    | HeadAggregate
    | HeadAggregateElement
    | HeadConditionalLiteral
    | HeadDisjunction
    | HeadSetAggregate
    | HeadSimpleLiteral
    | HeadTheoryAtom
    | LeftGuard
    | OptimizeElement
    | OptimizeTuple
    | ProgramPart
    | Projection
    | RightGuard
    | SetAggregateElement
    | TheoryAtomDefinition
    | TheoryAtomElement
    | TheoryGuardDefinition
    | TheoryOperatorDefinition
    | TheoryRightGuard
    | TheoryTermDefinition
    | TheoryTermFunction
    | TheoryTermSymbolic
    | TheoryTermTuple
    | TheoryTermUnparsed
    | TheoryTermVariable
    | UnparsedElement
)


AST_T = TypeVar(
    "AST_T",
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

FunctionLikeAST = TermFunction | TermSymbolic | TermTuple | Symbol


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

    def __str__(self) -> str:  # pragma: no cover
        return f"{self.location}: error: syntax error, {self.message}"


# class HeadBodyVisitor:

#     def __init__(self, library: Library):
#         self.lib = library

#     @singledispatchmethod
#     def _dispatch(self, expr: AST) -> None:
#         """
#         Order bodies of statements.
#         """
#         expr.visit(self.lib, self._dispatch)

#     @_dispatch.register
#     def _(self, node: ast.StatementRule, *args: Any, **kwargs: Any) -> None:
#         """
#         Visit the Rule node, setting the 'head' flag accordingly for children.

#         Parameters
#         ----------
#         node : AST
#             The Rule node.

#         Returns
#         -------
#         AST
#             The (potentially transformed) Rule node.
#         """
#         kwargs["head"] = True
#         node.head.visit(self._lib, self._dispatch)
#         kwargs["head"] = False
#         for element in node.body:
#             element.visit(self._lib, self._dispatch)


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
        self.errors: list[SyntacticError] = []

    def visit(self, node: AST, *args: Any, **kwargs: Any) -> None:
        """
        Visit the given AST node and check for invalid AST types.

        Parameters
        ----------
        node : AST
            The AST node to visit.
        """
        if type(node) in self.invalid_ASTTypes:
            assert hasattr(node, "location"), f"Node {node} has no location"
            self.errors.append(
                SyntacticError(node.location, f"unexpected {node}", type(node))
            )
        else:
            node.visit(self, args, kwargs)

    def __call__(self, node: AST, *args: Any, **kwargs: Any) -> None:
        self.visit(node, *args, **kwargs)


def create_literal(
    library: Library,
    atom: TermAST,
    sign: ast.Sign = ast.Sign.NoSign,
) -> LiteralAST:
    if hasattr(atom, "location"):
        location = atom.location
    else:  # pragma: no cover
        position = Position(library, "<aux>", 0, 0)
        location = Location(position, position)
    return ast.LiteralSymbolic(library, location, sign, atom)


# def create_head_literal(
#     library: Library,
#     atom: TermAST,
#     sign: ast.Sign = ast.Sign.NoSign,
# ) -> HeadSimpleLiteral:
#     """
#     Create a head literal from a term AST.

#     Args:
#         library (Library): The library to use for creating the literal.
#         atom (TermAST): The term AST to create the literal from.
#         sign (ast.Sign): The sign of the literal.

#     Returns:
#         HeadSimpleLiteral: The created head literal.
#     """
#     return ast.HeadSimpleLiteral(library, create_literal(library, atom, sign))


def create_body_literal(
    library: Library,
    atom: TermAST,
    sign: ast.Sign = ast.Sign.NoSign,
) -> BodySimpleLiteral:
    """
    Create a body literal from a term AST.

    Args:
        library (Library): The library to use for creating the literal.
        atom (TermAST): The term AST to create the literal from.
        sign (ast.Sign): The sign of the literal.

    Returns:
        BodySimpleLiteral: The created body literal.
    """
    return ast.BodySimpleLiteral(library, create_literal(library, atom, sign))


def is_function(node: AST) -> TypeIs[ast.TermFunction | ast.TermSymbolic]:
    return (
        isinstance(node, ast.TermFunction)
        or isinstance(node, ast.TermSymbolic)
        and node.symbol.type == SymbolType.Function
    )


def function_arguments(
    node: FunctionLikeAST,
) -> tuple[str, Sequence[ArgumentAST] | Sequence[Symbol]]:
    if isinstance(node, ast.TermTuple):
        name = ""
        assert len(node.pool) == 1 and isinstance(
            node.pool[0], ast.ArgumentTuple
        ), f"Terms must be unpooled {node}"
        arguments = node.pool[0].arguments
    elif isinstance(node, ast.TermFunction):
        name = node.name
        assert len(node.pool) == 1, f"Terms must be unpooled {node}"
        arguments = node.pool[0].arguments
    else:
        if isinstance(node, ast.TermSymbolic):
            node = node.symbol
        if node.type == SymbolType.Tuple:  # pragma: no cover
            name = ""
        else:
            assert (
                node.type == SymbolType.Function
            ), f"Expected a symbol function, got {node}: {node.type}"
            name = node.name
        arguments = node.arguments
    return name, arguments


def function_arguments_ast(
    library: Library,
    node: ast.TermFunction | ast.TermSymbolic,
) -> tuple[str, Sequence[TermAST]]:
    name, arguments = function_arguments(node)
    if arguments and isinstance(arguments[0], TermAST):
        return name, cast(Sequence[TermAST], arguments)
    return name, [
        ast.TermSymbolic(library, node.location, cast(Symbol, a)) for a in arguments
    ]


class FreshVariableGenerator:
    """
    Class to generate fresh variables given a set of already-used names.

    Usage:
        gen = FreshVariableGenerator(used_vars)
        v1 = gen.fresh_variable(lib, loc, "X")
    """

    def __init__(self, used: set[str] | None = None):
        self.used: set[str] = set(used) if used else set()

    def fresh_variable(
        self, lib: Library, location: Location, name: str = "V"
    ) -> ast.TermVariable:
        if name not in self.used:
            self.used.add(name)
            return ast.TermVariable(lib, location, name, False)

        for i in range(2, sys.maxsize):
            candidate = f"{name}{i}"
            if candidate not in self.used:
                self.used.add(candidate)
                return ast.TermVariable(lib, location, candidate, False)

        assert False, "This will never happen, but makes mypy happy"  # pragma: no cover


class ELibrary:

    def __init__(
        self,
        *,
        shared: bool = True,
        slotted: bool = True,
        log_level: LogLevel = LogLevel.Info,
        logger: typing.Callable[[MessageType, str], None] | None = None,
        message_limit: int = 25,
    ) -> None:
        self.error_messages: list[tuple[MessageType, str]] = []
        self.shared = shared
        self.slotted = slotted
        self.log_level = log_level
        self.logger = logger
        self.message_limit = message_limit
        self.library = Library(
            shared,
            slotted,
            log_level,
            self.logger_function,
            message_limit,
        )

    def logger_function(self, msg_type: MessageType, message: str) -> None:
        if self.logger is not None:  # pragma: no cover
            self.logger(msg_type, message)
        self.error_messages.append((msg_type, message))

    def __enter__(self) -> typing.Self:
        return self

    def __exit__(
        self, exc_type: typing.Any, exc_value: typing.Any, traceback: typing.Any
    ) -> bool:
        return self.library.__exit__(exc_type, exc_value, traceback)


class ParsingException(Exception):
    """
    Exception raised when parsing fails.
    """

    def __init__(self, errors: list[SyntacticError]) -> None:
        self.errors = errors
        messages = "\n".join(str(error) for error in errors)
        super().__init__(f"Parsing failed with {len(errors)} error(s):\n{messages}")


_PARSING_ERROR_RE = r"<(.*?)>:(\d+):(\d+)-(\d+): error: (.*)"
_PARSING_ERROR_PATTERN = re.compile(_PARSING_ERROR_RE)


def _process_error(
    library: Library, message: tuple[MessageType, str]
) -> SyntacticError:
    match = _PARSING_ERROR_PATTERN.match(message[1])
    if not match:  # pragma: no cover
        position = Position(library, "<unknown>", 0, 0)
        location = Location(position, position)
        msg = message[1]
    else:
        file, line, col_start, col_end, msg = match.groups()
        start = Position(library, file, int(line), int(col_start))
        end = Position(library, file, int(line), int(col_end))
        location = Location(start, end)
    return SyntacticError(
        location,
        msg,
    )


def parse_string(library: ELibrary, code: str) -> list[StatementAST]:
    """
    Parse a string into a list of AST statements.

    Args:
        library (Library): The library to use for parsing.
        code (str): The code string to parse.

    Returns:
        list[StatementAST]: The list of parsed AST statements.

    Raises:
        Raises ParsingError if parsing fails.
    """
    parsed = []
    # The error messages are stored to restore them after parsing
    # The library is set to have no error messages during parsing
    # This avoids mixing errors from previous operations with parsing errors
    # This errors will be returned in the ParsingError if parsing fails
    saved_errors = library.error_messages
    library.error_messages = []
    try:
        ast.parse_string(library.library, code, lambda stmt: parsed.append(stmt))
    except RuntimeError as e:
        if str(e) != "parsing failed":  # pragma: no cover
            raise e
        raise ParsingException(
            [_process_error(library.library, error) for error in library.error_messages]
        )
    finally:
        library.error_messages = saved_errors
    return parsed
