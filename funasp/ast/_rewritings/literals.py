"""
Term-level unnesting of intensional functions inside literals.

A nested intensional function term is replaced by a fresh ``FUN`` variable and
an equality comparison linking the term to the variable is recorded, to be
placed by the statement-level driver (``funasp.ast._rewritings.unnesting``).
"""

from functools import singledispatchmethod
from typing import Callable, List, Set, TypeVar

from clingo_funasp import ast, symbol
from clingo_funasp.core import Library, Location
from clingo_funasp.symbol import Symbol

from funasp.ast._rewritings.types import SymbolSignature
from funasp.util.ast import (
    FreshVariableGenerator,
    TermTransformer,
    make_equation,
)
from funasp.util.iterables import map_none

AST_T = TypeVar(
    "AST_T",
    ast.ArgumentTuple,
    ast.BodyAggregate,
    ast.BodyAggregateElement,
    ast.BodyConditionalLiteral,
    ast.BodySetAggregate,
    ast.BodySimpleLiteral,
    ast.HeadAggregate,
    ast.HeadAggregateElement,
    ast.HeadConditionalLiteral,
    ast.HeadDisjunction,
    ast.HeadSetAggregate,
    ast.HeadSimpleLiteral,
    ast.LeftGuard,
    ast.LiteralBoolean,
    ast.LiteralComparison,
    ast.LiteralSymbolic,
    ast.OptimizeElement,
    ast.OptimizeTuple,
    ast.RightGuard,
    ast.SetAggregateElement,
    ast.TermAbsolute,
    ast.TermBinaryOperation,
    ast.TermFunction,
    ast.TermSymbolic,
    ast.TermTuple,
    ast.TermUnaryOperation,
    ast.TermVariable,
)


def unnest_functions[
    T: (ast.LiteralBoolean | ast.LiteralComparison | ast.LiteralSymbolic)
](
    lib: Library,
    node: T,
    intensional_functions: Set[SymbolSignature],
    variable_generator: FreshVariableGenerator,
    *,
    outer: bool = True,
    sign: ast.Sign | None = None,
    unnest_left_guard_equality: bool = False,
    allowed_in_negated_literals: bool = True,
) -> tuple[T | None, List[ast.LiteralComparison]]:
    """
    Unnest intensional functions in a given rule and return the list of generated comparisons.
    """
    transformer = UnnestFunctionsInLiteralsTransformer(
        lib,
        intensional_functions,
        variable_generator,
        unnest_left_guard_equality,
        allowed_in_negated_literals,
    )

    new_node = transformer.unnest(
        node,
        outer,
        sign,
    )
    return new_node, transformer.unnested_functions


class UnnestFunctionsInLiteralsTransformer:
    """
    Recursively unnest intensional functions in Clingo AST.
    """

    def __init__(
        self,
        lib: Library,
        intensional_functions: Set[SymbolSignature],
        variable_generator: FreshVariableGenerator,
        unnest_left_guard_equality: bool = False,
        allowed_in_negated_literals: bool = True,
    ):
        """Initialize the literal unnesting transformer and its state."""
        self.lib = lib
        self.intensional_functions = intensional_functions
        self.var_gen = variable_generator
        self.unnested_functions: List[ast.LiteralComparison] = []
        self.unnest_left_guard_equality = unnest_left_guard_equality
        self.allowed_in_negated_literals = allowed_in_negated_literals

    def pop_all_unnested_functions(self) -> List[ast.LiteralComparison]:
        """Return and clear the comparisons generated during unnesting."""
        unnested = self.unnested_functions
        self.unnested_functions = []
        return unnested

    def _is_intensional(self, name: str, arity: int) -> bool:
        """Return whether the given function signature is intensional."""
        return SymbolSignature(name, arity) in self.intensional_functions

    def _is_intensional_term(self, term: ast.Term) -> bool:
        """Return whether the given term is an intensional function term."""
        print(f"Checking if term is intensional: {term} --- {type(term)}")
        if isinstance(term, ast.TermFunction):
            return any(
                self._is_intensional(term.name, len(p.arguments)) for p in term.pool
            )
        if (
            isinstance(term, ast.TermSymbolic)
            and term.symbol.type == symbol.SymbolType.Function
        ):
            return self._is_intensional(
                str(term.symbol.name), len(term.symbol.arguments)
            )
        return False

    @singledispatchmethod
    def unnest(
        self,
        node: AST_T,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> AST_T | None:
        """
        Unnest intensional functions in the given AST node.
        It returns a new node if changes were made, or None otherwise.
        """
        return node.transform(self.lib, self.unnest, outer, sign)

    @unnest.register
    def _(
        self,
        node: ast.LiteralSymbolic,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.LiteralSymbolic | None:
        """Unnest intensional functions inside a symbolic literal."""
        return node.transform(self.lib, self.unnest, outer=True, sign=node.sign)

    def _flip_equality(
        self,
        node: ast.LiteralComparison,
    ) -> ast.LiteralComparison:
        """
        Flip sides of an equality comparison.
        """
        left = node.right[0].term
        right = [node.right[0].update(self.lib, term=node.left)]
        return node.update(self.lib, left=left, right=right)

    @unnest.register
    def _(
        self,
        node: ast.LiteralComparison,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.LiteralComparison | None:
        """
        Normalize comparisons to have intensional functions on the left side of equality only
        """
        outer_left = False
        is_new_node = False
        # Special case: equality with a single right guard
        if len(node.right) == 1 and node.right[0].relation == ast.Relation.Equal:
            # Flip if intensional only on right-hand side
            if not self._is_intensional_term(node.left) and self._is_intensional_term(
                node.right[0].term
            ):
                node = self._flip_equality(node)
                is_new_node = True

            if not self.unnest_left_guard_equality:
                outer_left = True

        left = self.unnest(node.left, outer_left, sign=sign)
        right = map_none(
            lambda rg: rg.transform(self.lib, self.unnest, outer=False, sign=sign),
            node.right,
        )
        update = {}
        if left is not None:
            update["left"] = left
        if right is not None:
            update["right"] = right
        if not update:
            return node if is_new_node else None
        return node.update(self.lib, **update)

    def _transform_argument(
        self,
        node: ast.TermOrProjection | Symbol,
        depth: int,
        recursive_function: Callable[
            [ast.TermOrProjection, int, ast.Sign, Location], ast.TermOrProjection | None
        ],
        sign: ast.Sign,
        location: Location,
    ) -> ast.TermOrProjection | None:
        # print(
        #     f"Transforming argument: {node} --- {type(node)}, depth={depth}, sign={sign}"
        # )
        if depth <= 0:
            return None
        if isinstance(node, Symbol) and node.type == symbol.SymbolType.Function:
            if not self._is_intensional(node.name, len(node.arguments)):
                return None
            node = ast.TermSymbolic(self.lib, location, node)
        elif not isinstance(node, ast.TermFunction) or not any(
            self._is_intensional(node.name, len(t.arguments)) for t in node.pool
        ):
            return None
        # print(f"Unnesting intensional function: {node} --- {type(node)}...")
        node = recursive_function(node, depth + 1, sign, location) or node
        # print(f"Unnested intensional function: {node} --- {type(node)}")
        if not self.allowed_in_negated_literals and sign == ast.Sign.Single:
            raise RuntimeError(
                f"Intensional functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found '{str(node)}' at {location}."
            )
        fresh: ast.TermVariable = self.var_gen.fresh_variable(self.lib, location, "FUN")
        assert isinstance(node, ast.Term)
        comp = make_equation(self.lib, location, node, fresh, sign)
        self.unnested_functions.append(comp)
        return fresh

    @unnest.register
    def _(
        self,
        node: ast.TermOrProjection,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.TermOrProjection | None:
        """Unnest intensional function terms and replace inner calls with fresh variables."""
        depth = 0 if outer else 1
        sign = sign or ast.Sign.NoSign
        transformer = TermTransformer(
            self.lib,
            self._transform_argument,
        )
        return transformer(node, depth, sign, node.location)

    @unnest.register
    def _(
        self, node: ast.OptimizeTuple, outer: bool = True, sign: ast.Sign | None = None
    ) -> ast.OptimizeTuple | None:
        """Unnest intensional functions that occur inside optimize tuples."""
        return node.transform(self.lib, self.unnest, outer=False, sign=sign)
