from functools import singledispatchmethod
from typing import Iterable, List, Set, Tuple

from clingo import ast, symbol
from clingo.core import Library, Location

from fasp.syntax_tree._nodes import (
    FASP_AST,
    FASP_AST_T,
    AssignmentAST,
    HeadAggregateAssignment,
    HeadSimpleAssignment,
)
from fasp.syntax_tree.collectors import SymbolSignature
from fasp.util.ast import AST, AST_T, FreshVariableGenerator, TermAST


def unnest_functions(
    lib: Library,
    node: FASP_AST | Iterable[FASP_AST],
    evaluable_functions: Set[SymbolSignature],
    variable_generator: FreshVariableGenerator,
    *,
    outer: bool = True,
    sign: ast.Sign | None = None,
    unnest_left_guard_equality: bool = False,
    allowed_in_negated_literals: bool = True,
    # Might need to pass flag boolens like outer (already used downstream) and allow_evaluable_in_negative_literal (new)
) -> tuple[FASP_AST | list[FASP_AST], List[ast.LiteralComparison]]:
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

    if isinstance(node, FASP_AST):
        return transformer.transform_node(
            node,
            outer,
            sign,
        )
    nodes = [transformer._unnest(n) or n for n in node]
    return nodes, transformer.unnested_functions


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

    def transform_node(
        self,
        node: FASP_AST,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> Tuple[FASP_AST, List[ast.LiteralComparison]]:
        """
        Transform one AST node and return (new_node, unnested_list).

        - Resets the per-node list `self.unnested_functions` before transformation.
        - Keeps the same FreshVariableGenerator instance so successive calls across
          nodes in the same rule reuse the same variable namespace.
        """
        # Clear per-node collected unnested comparisons
        self.unnested_functions = []

        new_node = self._unnest(node, outer=outer, sign=None)

        # Copy the list and return
        collected = list(self.unnested_functions)
        return new_node or node, collected

    @singledispatchmethod
    def _unnest(
        self,
        node: FASP_AST_T,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> FASP_AST_T | None:
        """
        Unnest evaluable functions in the given AST node.
        It returns a new node if changes were made, or None otherwise.
        """
        return node.transform(self.lib, self._unnest, outer, sign)

    @_unnest.register
    def _(
        self,
        node: HeadSimpleAssignment,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> HeadSimpleAssignment:
        new_assigned = self._unnest(node.assigned_function, outer=True, sign=sign)
        new_value = self._unnest(node.value, outer=False, sign=sign)
        update = {}
        if new_assigned is not None:
            update["assigned_function"] = new_assigned
        if new_value is not None:
            update["value"] = new_value
        if not update:
            return node
        return node.update(self.lib, **update)

    @_unnest.register
    def _(
        self,
        node: ast.LiteralSymbolic,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.LiteralSymbolic:
        return node.transform(self.lib, self._unnest, outer=True, sign=node.sign)

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

    @_unnest.register
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

        new_left = self._unnest(node.left, outer_left, sign=sign)
        new_right = []
        is_new_right = False
        for rg in node.right:
            new_term = self._unnest(rg.term, outer=False, sign=sign)
            if new_term is not None:
                is_new_right = True
                new_right.append(rg.update(self.lib, term=new_term))
            else:
                new_right.append(rg)
        update = {}
        if new_left is not None:
            update["left"] = new_left
        if is_new_right:
            update["right"] = new_right
        if not update:
            return node if is_new_node else None
        return node.update(
            self.lib,
            **update,
        )

    @_unnest.register
    def _(
        self,
        node: ast.TermFunction | ast.TermSymbolic,
        outer: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.TermFunction | ast.TermSymbolic | ast.TermVariable | None:
        is_new_node = False
        if isinstance(node, ast.TermFunction):
            is_new_node = False
            pool = []
            for tup in node.pool:
                new_args: list[TermAST] = []
                for term in tup.arguments:
                    new_term = self._unnest(term, outer=False, sign=sign)
                    if new_term is not None:
                        is_new_node = True
                        new_args.append(new_term)
                    else:
                        new_args.append(term)
                pool.append(ast.ArgumentTuple(self.lib, new_args))
            if is_new_node:
                node = node.update(self.lib, pool=tuple(pool))
            name = node.name
            arguments = node.pool[0].arguments
        elif node.symbol.type != symbol.SymbolType.Function:
            return None
        else:
            new_args: list[TermAST] = []
            for term in node.symbol.arguments:
                new_term = self._unnest(term, outer=False, sign=sign)
                if new_term is not None:
                    is_new_node = True
                    new_args.append(new_term)
                else:
                    new_args.append(term)
            if is_new_node:
                node = node.update(
                    self.lib,
                    symbol=symbol.Function(self.lib, node.symbol.name, tuple(new_args)),
                )
            name = node.symbol.name
            arguments = node.symbol.arguments

        if not outer and self._is_evaluable(name, len(arguments)):
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
        return node if is_new_node else None

    @_unnest.register(
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
    )](self, node: T, outer: bool = True, sign: ast.Sign | None = None,) -> T | None:
        return node.transform(self.lib, self._unnest, outer=False, sign=sign)
