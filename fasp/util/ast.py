import sys
from typing import (
    AbstractSet,
    Any,
    Iterable,
    NamedTuple,
    Optional,
    Sequence,
    Set,
    TypeIs,
    TypeVar,
    cast,
    Union,
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
from clingo.core import Library, Location, Position
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

AST = (
    StatementAST
    | TermAST
    | LiteralAST
    | ArgumentTuple
    | BodyAggregate
    | BodyAggregateElement
    | BodyConditionalLiteral
    | BodySetAggregate
    | BodySimpleLiteral
    | BodyTheoryAtom
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

BodyLiteralAST = (
    BodySimpleLiteral
    | BodyConditionalLiteral
    | BodyAggregate
    | BodySetAggregate
    | BodyTheoryAtom
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
        if node.type == SymbolType.Tuple:
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
    return (
        name,
        [ast.TermSymbolic(library, node.location, cast(Symbol, a)) for a in arguments],
    )


class VariableCollector:
    """
    Class to collect variables from a list of AST statements.

    Usage:
        collector = VariableCollector()
        used_vars = collector.collect(statements)
    """

    def __init__(self):
        self.used: Set[str] = set()

    def collect(self, statements: Iterable[ast.StatementRule]) -> Set[str]:
        for stmt in statements:
            self._collect_vars(stmt)
        return self.used

    def _collect_vars(self, node: AST) -> None:
        if isinstance(node, ast.TermVariable):
            self.used.add(node.name)
            return
        node.visit(self._collect_vars)


def collect_variables(statements: Iterable[ast.StatementRule]) -> Set[str]:
    collector = VariableCollector()
    return collector.collect(statements)


class FreshVariableGenerator:
    """
    Class to generate fresh variables given a set of already-used names.

    Usage:
        gen = FreshVariableGenerator(used_vars)
        v1 = gen.fresh_variable(lib, loc, "X")
    """

    def __init__(self, used: Set[str] | None = None):
        self.used: Set[str] = set(used) if used else set()

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

class ComparisonCollector:
    """
    Collect all LiteralComparison nodes from an AST or iterable of ASTs.
    """

    def __init__(self):
        self.comparisons: Set[ast.LiteralComparison] = set()

    def collect(
        self, nodes: Union[Any, Iterable[Any]]
    ) -> Set[ast.LiteralComparison]:
        if isinstance(nodes, (list, tuple, set)):
            for node in nodes:
                self._collect(node) #pragma: no cover
        else:
            self._collect(nodes)
        return self.comparisons

    def _collect(self, node: Any) -> None:
        if isinstance(node, ast.LiteralComparison):
            self.comparisons.add(node)
        # recurse into children
        node.visit(self._collect)

def collect_comparisons(
    node: AST, 
) -> Set[ast.LiteralComparison]:
    """Collect all LiteralComparison nodes from an AST into the provided set."""
    collector = ComparisonCollector()
    out: Set[ast.LiteralComparison] = set()
    comps = collector.collect(node)
    return comps
