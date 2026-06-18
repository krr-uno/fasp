"""
Term-level unnesting of intensional functions inside literals.

A nested intensional function term is replaced by a fresh ``FUN`` variable and
an equality comparison linking the term to the variable is recorded, to be
placed by the statement-level driver (``funasp.ast._rewritings.unnesting``).
"""

from functools import singledispatchmethod
from typing import List, Sequence, Set, TypeVar

from clingo_funasp import ast, symbol
from clingo_funasp.core import Library, Location
from clingo_funasp.symbol import Symbol

from funasp.astt._rewritings.types import SymbolSignature
from funasp.util.ast import (
    FreshVariableGenerator,
    is_function,
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
        if isinstance(term, ast.TermFunction):
            return self._is_intensional(term.name, len(term.pool[0].arguments))
        if (
            isinstance(term, ast.TermSymbolic)
            and term.symbol.type == symbol.SymbolType.Function
        ):
            return self._is_intensional(
                str(term.symbol.name), len(term.symbol.arguments)
            )
        return False

    def _make_comparison(
        self,
        loc: Location,
        left: ast.Term,
        right: ast.Term,
        sign: ast.Sign | None = None,
    ) -> ast.LiteralComparison:
        """Build an equality comparison linking an unnested function to a fresh variable."""
        return ast.LiteralComparison(
            self.lib,
            loc,
            ast.Sign.Double if sign == ast.Sign.Double else ast.Sign.NoSign,
            left,
            [ast.RightGuard(self.lib, ast.Relation.Equal, right)],
        )

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

    def _unnest_symbol_function(
        self,
        node: Symbol,
        sign: ast.Sign | None,
        location: Location,
    ) -> tuple[ast.TermFunction | None, str, Sequence[Symbol] | Sequence[ast.Term]]:
        """Unnest intensional functions appearing inside a symbolic function argument list."""
        arguments: list[Symbol | ast.Term] = []
        has_new_argument = False
        for arg in node.arguments:
            new_arg = self.unnest(arg, False, sign, location)
            if new_arg is not None:
                arguments.append(new_arg)
                has_new_argument = True
            else:
                arguments.append(arg)
        if not has_new_argument:
            return None, node.name, node.arguments
        new_arguments = [
            (
                arg
                if not isinstance(arg, Symbol)
                else ast.TermSymbolic(self.lib, location, arg)
            )
            for arg in arguments
        ]
        new_node = ast.TermFunction(
            self.lib,
            location,
            node.name,
            [ast.ArgumentTuple(self.lib, new_arguments)],
            external=False,
        )
        return new_node, node.name, new_arguments

    @unnest.register
    def _(
        self,
        node: ast.TermFunction | ast.TermSymbolic | Symbol,
        outer: bool = True,
        sign: ast.Sign | None = None,
        location: Location | None = None,
    ) -> ast.TermFunction | ast.TermSymbolic | ast.TermVariable | None:
        """Unnest intensional function terms and replace inner calls with fresh variables."""

        if not isinstance(node, Symbol) and not is_function(node):
            return None

        if isinstance(node, ast.TermSymbolic):
            return self.unnest(
                node.symbol, outer=outer, sign=sign, location=node.location
            )
        elif isinstance(node, Symbol):
            assert location is not None, "Location must be provided for Symbol nodes"
            if node.type != symbol.SymbolType.Function:
                return None
            new_node, name, arguments = self._unnest_symbol_function(
                node, sign, location
            )
            if outer or not self._is_intensional(name, len(arguments)):
                return new_node
        else:
            new_node = node.transform(
                self.lib,
                self.unnest,
                outer=False,
                sign=sign,
            )
            if outer:
                return new_node
            name = node.name
            for arguments in node.pool:
                if self._is_intensional(name, len(arguments.arguments)):
                    break
            else:
                return new_node
            location = node.location

        node = new_node or node

        if not self.allowed_in_negated_literals and sign == ast.Sign.Single:
            raise RuntimeError(
                f"Intensional functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found '{str(node)}' at {location}."
            )
        fresh: ast.TermVariable = self.var_gen.fresh_variable(self.lib, location, "FUN")
        if isinstance(node, Symbol):
            node = ast.TermSymbolic(self.lib, location, node)
        comp = self._make_comparison(location, node, fresh, sign=sign)
        self.unnested_functions.append(comp)
        return fresh

    @unnest.register(
        ast.TermAbsolute
        | ast.TermUnaryOperation
        | ast.TermBinaryOperation
        | ast.TermTuple
    )
    def _[
        T: (
            ast.TermAbsolute,
            ast.TermUnaryOperation,
            ast.TermBinaryOperation,
            ast.TermTuple,
        )
    ](
        self, node: T, outer: bool = True, sign: ast.Sign | None = None
    ) -> T | None:
        """Unnest intensional functions that occur inside composite term nodes."""
        return node.transform(self.lib, self.unnest, outer=False, sign=sign)

    @unnest.register
    def _(
        self, node: ast.OptimizeTuple, outer: bool = True, sign: ast.Sign | None = None
    ) -> ast.OptimizeTuple | None:
        """Unnest intensional functions that occur inside optimize tuples."""
        return node.transform(self.lib, self.unnest, outer=False, sign=sign)
