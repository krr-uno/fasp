import sys
from functools import singledispatchmethod
from typing import (
    AbstractSet,
    Any,
    Callable,
    NamedTuple,
    Optional,
    Sequence,
    TypeIs,
)

from clingo_funasp import ast
from clingo_funasp.core import Library, Location, Position
from clingo_funasp.symbol import Symbol, SymbolType

AST = (
    ast.Statement
    | ast.Term
    | ast.Literal
    | ast.ArgumentTuple
    | ast.BodyLiteral
    | ast.BodyAggregateElement
    | ast.Edge
    | ast.HeadAggregateElement
    | ast.HeadLiteral
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


# AST_T = TypeVar(
#     "AST_T",
#     ast.ArgumentTuple,
#     ast.BodyAggregate,
#     ast.BodyAggregateElement,
#     ast.BodyConditionalLiteral,
#     ast.BodySetAggregate,
#     ast.BodySimpleLiteral,
#     ast.BodyTheoryAtom,
#     ast.Edge,
#     ast.HeadAggregate,
#     ast.HeadAggregateElement,
#     ast.HeadConditionalLiteral,
#     ast.HeadDisjunction,
#     ast.HeadSetAggregate,
#     ast.HeadSimpleLiteral,
#     ast.HeadTheoryAtom,
#     ast.LeftGuard,
#     ast.LiteralBoolean,
#     ast.LiteralComparison,
#     ast.LiteralSymbolic,
#     ast.OptimizeElement,
#     ast.OptimizeTuple,
#     ast.ProgramPart,
#     ast.Projection,
#     ast.RightGuard,
#     ast.SetAggregateElement,
#     ast.StatementComment,
#     ast.StatementConst,
#     ast.StatementDefined,
#     ast.StatementEdge,
#     ast.StatementExternal,
#     ast.StatementHeuristic,
#     ast.StatementInclude,
#     ast.StatementOptimize,
#     ast.StatementParts,
#     ast.StatementProgram,
#     ast.StatementProject,
#     ast.StatementProjectSignature,
#     ast.StatementRule,
#     ast.StatementScript,
#     ast.StatementShow,
#     ast.StatementShowNothing,
#     ast.StatementShowSignature,
#     ast.StatementTheory,
#     ast.StatementWeakConstraint,
#     ast.TermAbsolute,
#     ast.TermBinaryOperation,
#     ast.TermFunction,
#     ast.TermSymbolic,
#     ast.TermTuple,
#     ast.TermUnaryOperation,
#     ast.TermVariable,
#     ast.TheoryAtomDefinition,
#     ast.TheoryAtomElement,
#     ast.TheoryGuardDefinition,
#     ast.TheoryOperatorDefinition,
#     ast.TheoryRightGuard,
#     ast.TheoryTermDefinition,
#     ast.TheoryTermFunction,
#     ast.TheoryTermSymbolic,
#     ast.TheoryTermTuple,
#     ast.TheoryTermUnparsed,
#     ast.TheoryTermVariable,
#     ast.UnparsedElement,
# )

FunctionLikeAST = ast.TermFunction | ast.TermSymbolic | ast.TermTuple | Symbol


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
        """Return the clingo-style string representation of this syntax error."""
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
        """Visit the given node when the visitor is used as a callable."""
        self.visit(node, *args, **kwargs)


def create_literal(
    library: Library,
    atom: ast.Term,
    sign: ast.Sign = ast.Sign.NoSign,
) -> ast.Literal:
    """Create a symbolic literal from the given term."""
    if hasattr(atom, "location"):
        location = atom.location
    else:  # pragma: no cover
        position = Position(library, "<aux>", 0, 0)
        location = Location(position, position)
    return ast.LiteralSymbolic(library, location, sign, atom)


# def create_head_literal(
#     library: Library,
#     atom: ast.Term,
#     sign: ast.Sign = ast.Sign.NoSign,
# ) -> HeadSimpleLiteral:
#     """
#     Create a head literal from a term AST.

#     Args:
#         library (Library): The library to use for creating the literal.
#         atom (ast.Term): The term AST to create the literal from.
#         sign (ast.Sign): The sign of the literal.

#     Returns:
#         HeadSimpleLiteral: The created head literal.
#     """
#     return ast.HeadSimpleLiteral(library, create_literal(library, atom, sign))


def create_body_literal(
    library: Library,
    atom: ast.Term,
    sign: ast.Sign = ast.Sign.NoSign,
) -> ast.BodySimpleLiteral:
    """
    Create a body literal from a term AST.

    Args:
        library (Library): The library to use for creating the literal.
        atom (ast.Term): The term AST to create the literal from.
        sign (ast.Sign): The sign of the literal.

    Returns:
        BodySimpleLiteral: The created body literal.
    """
    return ast.BodySimpleLiteral(library, create_literal(library, atom, sign))


def is_function(node: AST) -> TypeIs[ast.TermFunction | ast.TermSymbolic]:
    """Return whether the given AST node represents a function term."""
    return isinstance(node, ast.TermFunction) or (
        isinstance(node, ast.TermSymbolic) and node.symbol.type == SymbolType.Function
    )


def function_arguments(
    node: FunctionLikeAST,
) -> tuple[str, Sequence[ast.TermOrProjection] | Sequence[Symbol]]:
    """Return the function name and positional arguments for a function-like node."""
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
) -> tuple[str, Sequence[ast.TermOrProjection]]:
    """Return function arguments as AST terms for the given function-like node."""
    if isinstance(node, ast.TermFunction) and len(node.pool) > 1:
        # Preserve pooled alternatives as one tuple argument: f(1;a,b) -> f((1;a,b)).
        return node.name, [ast.TermTuple(library, node.location, list(node.pool))]

    name, arguments = function_arguments(node)
    new_arguments: list[ast.TermOrProjection] = []
    for a in arguments:
        if isinstance(a, Symbol):
            new_arguments.append(ast.TermSymbolic(library, node.location, a))
        else:
            new_arguments.append(a)

    return name, new_arguments

    # if arguments and isinstance(arguments[0], ast.Term):
    #     return name, cast(Sequence[ast.Term], arguments)
    # return name, [
    #     ast.TermSymbolic(library, node.location, cast(Symbol, a)) for a in arguments
    # ]


class FreshVariableGenerator:
    """
    Class to generate fresh variables given a set of already-used names.

    Usage:
        gen = FreshVariableGenerator(used_vars)
        v1 = gen.fresh_variable(lib, loc, "X")
    """

    def __init__(self, used: set[str] | None = None):
        """Initialize the generator with the set of already-used variable names."""
        self.used: set[str] = set(used) if used else set()

    def fresh_variable(
        self, lib: Library, location: Location, name: str = "V"
    ) -> ast.TermVariable:
        """Create a fresh variable name that does not clash with previously used names."""
        if name not in self.used:
            self.used.add(name)
            return ast.TermVariable(lib, location, name, False)

        for i in range(2, sys.maxsize):
            candidate = f"{name}{i}"
            if candidate not in self.used:
                self.used.add(candidate)
                return ast.TermVariable(lib, location, candidate, False)

        assert False, "This will never happen, but makes mypy happy"  # pragma: no cover


class ParsingException(Exception):
    """
    Exception raised when parsing fails.
    """

    def __init__(self, errors: list[SyntacticError]) -> None:
        """Initialize the exception with the collected syntactic errors."""
        self.errors = errors
        messages = "\n".join(str(error) for error in errors)
        super().__init__(f"Parsing failed with {len(errors)} error(s):\n{messages}")


TRANSFORM_FUNC = Callable[[ast.TermOrProjection | Symbol], ast.TermOrProjection | None]


class TermTransformer:

    def __init__(
        self,
        library: Library,
        transform_func: TRANSFORM_FUNC,
    ):
        self.library = library
        self.transform_func = transform_func
        self._last_location: Location = Location(
            Position(library, "<unknown>", 0, 0), Position(library, "<unknown>", 0, 0)
        )

    def apply_to_symbol(
        self, symbol: Symbol, location: Location
    ) -> ast.TermOrProjection | None:
        """
        Transform a Symbol using the provided transformation function. If the Symbol is replaced by a Term, it will be wrapped in a TermSymbolic node.

        Args:
            library (Library): The library to use for creating new AST nodes.
            symbol (Symbol): The Symbol to transform.
        """
        transformed = self.transform_func(symbol)
        if transformed is not None or symbol.type != SymbolType.Function:
            return transformed
        all_none = True
        new_arguments: list[ast.TermOrProjection | Symbol | None] = []
        for arg in symbol.arguments:
            transformed_arg = self.apply_to_symbol(arg, location)
            new_arguments.append(transformed_arg)
            if transformed_arg is not None:
                all_none = False
        if all_none:
            return None
        new_arguments2: list[ast.TermOrProjection] = []
        for new, old in zip(new_arguments, symbol.arguments):
            new = new or old
            if isinstance(new, Symbol):
                new = ast.TermSymbolic(self.library, location, new)
            new_arguments2.append(new)
        arguments_tuple = ast.ArgumentTuple(self.library, new_arguments2)
        return ast.TermFunction(self.library, location, symbol.name, [arguments_tuple])

    @singledispatchmethod
    def __call__(
        self,
        term: ast.TermOrProjection | ast.ArgumentTuple | Symbol,
    ) -> ast.TermOrProjection | ast.ArgumentTuple | None:
        """
        Transform a term AST node using the provided transformation function. If a Symbol is replaced by a Term, it will be wrapped in a TermSymbolic node and the tree will be accordingly transformed.

        Args:
            library (Library): The library to use for creating new AST nodes.
            term (ast.Term): The term AST node to transform.
            transform_func (callable): A function that takes a term and returns a transformed term.

        Returns:
            ast.Term: The transformed term AST node.
        """
        raise NotImplementedError(
            f"Unsupported term type: {type(term)}"
        )  # pragma: no cover

    @__call__.register
    def _(self, term: ast.TermOrProjection | Symbol) -> ast.TermOrProjection | None:
        if isinstance(term, Symbol):
            return self.apply_to_symbol(term, self._last_location)
        self._last_location = term.location
        new_term = self.transform_func(term)
        if new_term is not None:
            return new_term
        return term.transform(self.library, self.__call__)

    @__call__.register
    def _(self, term: ast.ArgumentTuple) -> ast.ArgumentTuple | None:
        return term.transform(self.library, self.__call__)
