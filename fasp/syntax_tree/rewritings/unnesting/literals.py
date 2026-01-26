from functools import singledispatchmethod
from typing import List, Set

from clingo import ast, symbol
from clingo.core import Library, Location

from fasp.syntax_tree._nodes import (
    FASP_AST,
    FASP_AST_T,
    AssignmentAST,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from fasp.syntax_tree.collectors import SymbolSignature
from fasp.util.ast import AST, AST_T, FreshVariableGenerator, TermAST
from fasp.util.iterables import map_none


def unnest_functions[T: (
    ast.LiteralBoolean | ast.LiteralComparison | ast.LiteralSymbolic
)](
    lib: Library,
    node: T,
    evaluable_functions: Set[SymbolSignature],
    variable_generator: FreshVariableGenerator,
    *,
    outer: bool = True,
    sign: ast.Sign | None = None,
    unnest_left_guard_equality: bool = False,
    allowed_in_negated_literals: bool = True,
) -> tuple[T | None, List[ast.LiteralComparison]]:
    """
    Unnest evaluable functions in a given rule and return the list of generated comparisons.
    """
    transformer = UnnestFunctionsInLiteralsTransformer(
        lib,
        evaluable_functions,
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
    Recursively unnest evaluable functions in Clingo AST.
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
        variable_generator: FreshVariableGenerator,
        unnest_left_guard_equality: bool = False,
        allowed_in_negated_literals: bool = True,
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.var_gen = variable_generator
        self.unnested_functions: List[ast.LiteralComparison] = []
        self.unnest_left_guard_equality = unnest_left_guard_equality
        self.allowed_in_negated_literals = allowed_in_negated_literals

    def pop_all_unnested_functions(self) -> List[ast.LiteralComparison]:
        unnested = self.unnested_functions
        self.unnested_functions = []
        return unnested

    def _is_evaluable(self, name: str, arity: int) -> bool:
        return SymbolSignature(name, arity) in self.evaluable_functions

    def _is_evaluable_term(self, term: TermAST) -> bool:
        if isinstance(term, ast.TermFunction):
            return self._is_evaluable(term.name, len(term.pool[0].arguments))
        if (
            isinstance(term, ast.TermSymbolic)
            and term.symbol.type == symbol.SymbolType.Function
        ):
            return self._is_evaluable(str(term.symbol.name), len(term.symbol.arguments))
        return False

    def _make_comparison(
        self, loc: Location, left: TermAST, right: TermAST, sign: ast.Sign | None = None
    ) -> ast.LiteralComparison:
        return ast.LiteralComparison(
            self.lib,
            loc,
            # ast.Sign.NoSign if sign is None else sign,
            ast.Sign.Double if sign == ast.Sign.Double else ast.Sign.NoSign,
            left,
            [ast.RightGuard(self.lib, ast.Relation.Equal, right)],
        )

    @singledispatchmethod
    def unnest(
        self,
        node: FASP_AST_T,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> FASP_AST_T | None:
        """
        Unnest evaluable functions in the given AST node.
        It returns a new node if changes were made, or None otherwise.
        """
        return node.transform(self.lib, self.unnest, outer, sign)

    @unnest.register
    def _(
        self,
        node: HeadSimpleAssignment,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> HeadSimpleAssignment:
        new_assigned = self.unnest(node.assigned_function, outer=True, sign=sign)
        new_value = self.unnest(node.value, outer=False, sign=sign)
        update = {}
        if new_assigned is not None:
            update["assigned_function"] = new_assigned
        if new_value is not None:
            update["value"] = new_value
        if not update:
            return node
        return node.update(self.lib, **update)

    @unnest.register
    def _(
        self,
        node: ast.LiteralSymbolic,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.LiteralSymbolic | None:
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
        Normalize comparisons to have evaluable functions on the left side of equality only
        """
        outer_left = False
        is_new_node = False
        # Special case: equality with a single right guard
        if len(node.right) == 1 and node.right[0].relation == ast.Relation.Equal:
            # Flip if evaluable only on right-hand side
            if not self._is_evaluable_term(node.left) and self._is_evaluable_term(
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

    @unnest.register
    def _(
        self,
        node: ast.TermFunction | ast.TermSymbolic,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.TermFunction | ast.TermSymbolic | ast.TermVariable | None:
        if (
            isinstance(node, ast.TermSymbolic)
            and node.symbol.type != symbol.SymbolType.Function
        ):
            return None

        new_node = node.transform(
            self.lib,
            self.unnest,
            outer=False,
            sign=sign,
        )
        if isinstance(node, ast.TermFunction):
            name = node.name
            arguments = node.pool[0].arguments
        else:
            name = node.symbol.name
            arguments = node.symbol.arguments

        if outer or not self._is_evaluable(name, len(arguments)):
            return new_node

        node = new_node or node

        if not self.allowed_in_negated_literals and sign == ast.Sign.Single:
            raise RuntimeError(
                f"Evaluable functions are not allowed in negated literals in conditions of aggregates and conditional literals. Found '{str(node)}' at {node.location}."
            )
        fresh: ast.TermVariable = self.var_gen.fresh_variable(
            self.lib, node.location, "FUN"
        )
        comp = self._make_comparison(node.location, node, fresh, sign=sign)
        self.unnested_functions.append(comp)
        return fresh

    @unnest.register(
        ast.TermAbsolute
        | ast.TermUnaryOperation
        | ast.TermBinaryOperation
        | ast.TermTuple
    )
    def _[T: (
        ast.TermAbsolute,
        ast.TermUnaryOperation,
        ast.TermBinaryOperation,
        ast.TermTuple,
    )](self, node: T, outer: bool = True, sign: ast.Sign | None = None) -> T | None:
        return node.transform(self.lib, self.unnest, outer=False, sign=sign)
