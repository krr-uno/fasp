from ast import arg
from doctest import COMPARISON_FLAGS
from functools import singledispatchmethod
from inspect import signature
from itertools import chain
import re
from typing import AbstractSet, Any, Iterable, cast
from click import Argument
from clingo import ast
from clingo.core import Location, Position, Library
from clingo.symbol import Symbol, Number, SymbolType

from fasp.util.ast import AST

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
    def dispatch(self, node: AST) -> AST:
        return node.transform(self.library, self.dispatch) or node

    @dispatch.register
    def _(self, node: ast.LiteralComparison) -> ast.LiteralSymbolic:
        return self.protect_comparison(node)

    @dispatch.register
    def _(
        self, node: ast.LiteralBoolean | ast.LiteralSymbolic
    ) -> ast.LiteralBoolean | ast.LiteralSymbolic:
        return node

    def rewrite(self, node: AST) -> AST:
        if not isinstance(node, ast.StatementRule):
            return node
        return node.transform(self.library, self.dispatch) or node


def protect_comparisons(library: Library, statements: Iterable[AST]) -> Iterable[AST]:
    """
    Protect comparisons in a Clingo AST.

    Args:
        statements (Iterable[AST]): The AST statements to protect.

    Returns:
        Iterable[AST]: The protected AST statements.
    """
    transformer = _ComparisonProtectorTransformer(library)
    return (transformer.rewrite(statement) for statement in statements)


def _restore_guard(library: Library, term: ast.TermFunction) -> ast.RightGuard:
    arguments = term.pool[0].arguments
    term1 = arguments[0]
    assert isinstance(
        term1, ast.TermSymbolic
    ), f"Expected a symbolic term, got {term1}: {type(term1)}"
    relation_int = term1.symbol
    assert isinstance(
        relation_int, Symbol
    ), f"Expected a symbol, got {relation_int}: {type(relation_int)}"
    assert (
        relation_int.type == SymbolType.Number
    ), f"Expected a number, got {relation_int}: {relation_int.type}"
    term2 = arguments[1]
    assert not isinstance(
        term2, ast.Projection
    ), f"Expected a non-tuple term, got {term2}: {type(term2)}"
    return ast.RightGuard(library, INT_TO_RELATION[relation_int.number], term2)


def restore_comparison(
    library: Library,
    literal: ast.LiteralSymbolic,
    comparison_name: str = COMPARISON_NAME,
) -> ast.LiteralSymbolic | ast.LiteralComparison:
    atom = literal.atom
    assert isinstance(
        atom, ast.TermFunction
    ), f"Expected a function term, got {atom}: {type(atom)}"
    arguments = atom.pool[0].arguments
    if atom.name != comparison_name:
        return literal
    left = arguments[0]
    assert not isinstance(left, ast.Projection)
    argument = arguments[1]
    assert isinstance(
        argument, ast.TermTuple
    ), f"Expected a tuple term, got {argument}: {type(argument)}"
    right = [
        _restore_guard(library, cast(ast.TermFunction, g))
        for g in cast(ast.ArgumentTuple, argument.pool[0]).arguments
    ]
    assert isinstance(arguments[2], ast.TermSymbolic)
    sign = INT_TO_SIGN[arguments[2].symbol.number]
    return ast.LiteralComparison(library, literal.location, sign, left, right)
