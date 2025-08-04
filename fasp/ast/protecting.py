from ast import arg
from functools import singledispatchmethod
from typing import Iterable, Sequence, cast
from dataclasses import dataclass

from clingo import ast
from clingo.core import Location, Position, Library
from clingo.symbol import Symbol, Number, SymbolType

from fasp import symbol
from fasp.util.ast import AST, AST_T, StatementAST, TermAST, function_arguments_ast, is_function

INT_TO_SIGN = [
    ast.Sign.NoSign,
    ast.Sign.Single,
    ast.Sign.Double,
]

INT_TO_RELATION = [
    ast.Relation.Equal,
    ast.Relation.Greater,
    ast.Relation.GreaterEqual,
    ast.Relation.Less,
    ast.Relation.LessEqual,
    ast.Relation.NotEqual,
]

SIGN_TO_INT = {r: i for i, r in enumerate(INT_TO_SIGN)}

RELATION_TO_INT = {r: i for i, r in enumerate(INT_TO_RELATION)}

COMPARISON_NAME = "CMP"
GUARD_NAME = "GRD"


class ComparisonProtector:
    """
    A class to protect comparisons in a Clingo AST.
    """

    def __init__(
        self,
        library: Library,
        comparison_name: str = COMPARISON_NAME,
        guard_name: str = GUARD_NAME,
    ):
        self.library = library
        self.comparison_name = comparison_name
        self.guard_name = guard_name
        position = Position(library, "<aux>", 0, 0)
        location = Location(position, position)
        self.sign_to_int = {
            r: ast.TermSymbolic(library, location, Number(library, n))
            for r, n in SIGN_TO_INT.items()
        }
        self.relation_to_int = {
            r: ast.TermSymbolic(library, location, Number(library, n))
            for r, n in RELATION_TO_INT.items()
        }

    def _guard_to_function(
        self, location: Location, guard: ast.RightGuard
    ) -> ast.TermFunction:
        arguments = ast.ArgumentTuple(
            self.library, [self.relation_to_int[guard.relation], guard.term]
        )
        return ast.TermFunction(self.library, location, self.guard_name, [arguments])

    def protect_comparison(
        self, comparison: ast.LiteralComparison
    ) -> ast.LiteralSymbolic:
        """
        Rewrites a LiteralComparison as a positive LiteralSymbolic of the form
            Comparison(left, right, sign)
        where right is a tuple of functions symbols Guard(relation, term)
        """
        location = comparison.location
        sign = self.sign_to_int[comparison.sign]
        left = comparison.left
        right = ast.TermTuple(
            self.library,
            location,
            [
                ast.ArgumentTuple(
                    self.library,
                    [self._guard_to_function(location, g) for g in comparison.right],
                )
            ],
        )
        atom = ast.TermFunction(
            self.library,
            location,
            self.comparison_name,
            [ast.ArgumentTuple(self.library, [left, right, sign])],
        )
        return ast.LiteralSymbolic(self.library, location, ast.Sign.NoSign, atom)

    def __call__(self, comparison: ast.LiteralComparison) -> ast.LiteralSymbolic:
        """
        Call method to protect a comparison.
        """
        return self.protect_comparison(comparison)


class _ComparisonProtectorTransformer:
    """
    A transformer to protect comparisons in a Clingo AST.
    """

    def __init__(self, library: Library):
        self.library = library
        self.protect_comparison = ComparisonProtector(library)

    @singledispatchmethod
    def dispatch(self, node: AST_T) -> AST_T:
        return node.transform(self.library, self.dispatch) or node

    @dispatch.register
    def _(self, node: ast.LiteralComparison) -> ast.LiteralSymbolic:
        return self.protect_comparison(node)

    @dispatch.register
    def _(
        self, node: ast.LiteralBoolean | ast.LiteralSymbolic
    ) -> ast.LiteralBoolean | ast.LiteralSymbolic:
        return node

    def rewrite(self, node: StatementAST) -> StatementAST:
        if not isinstance(node, ast.StatementRule):
            return node
        return node.transform(self.library, self.dispatch) or node


def protect_comparisons(
    library: Library, statements: Iterable[StatementAST]
) -> Iterable[StatementAST]:
    """
    Protect comparisons in a Clingo AST.

    Args:
        statements (Iterable[AST]): The AST statements to protect.

    Returns:
        Iterable[AST]: The protected AST statements.
    """
    transformer = _ComparisonProtectorTransformer(library)
    return (transformer.rewrite(statement) for statement in statements)


@dataclass
class RightGuard:
    """
    A class to represent a right guard in a comparison.

    Attributes:
        relation (ast.Relation): The relation of the guard.
        term (TermAST): The term associated with the guard.
    """
    location: Location
    relation: ast.Relation
    term: TermAST | symbol.Symbol

    def to_ast(self, library: Library) -> ast.RightGuard:
        """
        Convert the RightGuard to an AST RightGuard.

        Args:
            library (Library): The Clingo library.

        Returns:
            ast.RightGuard: The AST representation of the right guard.
        """
        if isinstance(self.term, symbol.Symbol):
            term = ast.TermSymbolic(library, self.location, self.term)
        else:
            term = self.term
        return ast.RightGuard(library, self.relation, term)


def _restore_guard_arguments(location: Location, term: ast.TermFunction | symbol.Symbol) -> RightGuard:
    if isinstance(term, symbol.Symbol):
        relation_int = term.arguments[0]
        term2 = term.arguments[1]
    else:
        relation_int = term.pool[0].arguments[0].symbol
        term2 = term.pool[0].arguments[1]
        location = term.location
    # assert type(
    #     term1, ast.TermSymbolic
    # ), f"Expected a symbolic term, got {term1}: {type(term1)}"
    # relation_int = term1.symbol
    assert isinstance(
        relation_int, Symbol
    ), f"Expected a symbol, got {relation_int}: {type(relation_int)}"
    assert (
        relation_int.type == SymbolType.Number
    ), f"Expected a number, got {relation_int}: {relation_int.type}"
    # term2 = arguments[1]
    assert not isinstance(
        term2, ast.Projection
    ), f"Expected a non-tuple term, got {term2}: {type(term2)}"
    return RightGuard(location, INT_TO_RELATION[relation_int.number], term2)


def restore_comparison_arguments(
    location: Location,
    arguments: Sequence[TermAST],
) -> tuple[ast.Sign, TermAST, list[RightGuard]]:
    assert (
        len(arguments) == 3
    ), f"Expected 3 arguments, got {len(arguments)}: {arguments}"
    left = arguments[0]
    assert not isinstance(left, ast.Projection)
    assert isinstance(arguments[2], ast.TermSymbolic)
    sign = INT_TO_SIGN[arguments[2].symbol.number]
    argument_rigth = arguments[1]
    assert type(argument_rigth) in {ast.TermTuple, ast.TermSymbolic}, f"Expected a tuple term, got {argument_rigth}: {type(argument_rigth)}"
    if isinstance(argument_rigth, ast.TermSymbolic):
        # If it's a symbolic term, it should be a tuple with one argument
        assert argument_rigth.symbol.type == SymbolType.Tuple
        guards = argument_rigth.symbol.arguments
    else:
        assert isinstance(argument_rigth, ast.TermTuple)
        guards = argument_rigth.pool[0].arguments
    right = [
        _restore_guard_arguments(location, g)
        for g in guards
    ]
    return sign, left, right


def restore_comparison(
    library: Library,
    literal: ast.LiteralSymbolic,
    comparison_name: str = COMPARISON_NAME,
) -> ast.LiteralSymbolic | ast.LiteralComparison:
    atom = literal.atom
    # if not isinstance(atom, ast.TermFunction) or atom.name != comparison_name:
    #     return literal
    if not is_function(atom):
        return literal
    function_name, arguments = function_arguments_ast(library, atom) 
    if function_name != comparison_name:
        return literal
    sign, left, right = restore_comparison_arguments(literal.location, arguments)
    ast_right = [r.to_ast(library) for r in right]
    return ast.LiteralComparison(library, literal.location, sign, left, ast_right)


class _ComparisonRestorationTransformer:
    """
    A transformer to restore comparisons in a Clingo AST.
    """

    def __init__(self, library: Library):
        self.library = library
        self.protect_comparison = ComparisonProtector(library)

    @singledispatchmethod
    def dispatch(self, node: AST) -> AST:
        return node.transform(self.library, self.dispatch) or node

    @dispatch.register
    def _(
        self, node: ast.LiteralSymbolic
    ) -> ast.LiteralSymbolic | ast.LiteralComparison:
        return restore_comparison(self.library, node)

    @dispatch.register
    def _(
        self, node: ast.LiteralBoolean | ast.LiteralComparison
    ) -> ast.LiteralBoolean | ast.LiteralComparison:
        return node

    def rewrite(self, node: StatementAST) -> StatementAST:
        if not isinstance(node, ast.StatementRule):
            return node
        return node.transform(self.library, self.dispatch) or node


def restore_comparisons(
    library: Library, statements: Iterable[StatementAST]
) -> Iterable[StatementAST]:
    """
    Protect comparisons in a Clingo AST.

    Args:
        statements (Iterable[AST]): The AST statements to protect.

    Returns:
        Iterable[AST]: The protected AST statements.
    """
    transformer = _ComparisonRestorationTransformer(library)
    return (transformer.rewrite(statement) for statement in statements)
