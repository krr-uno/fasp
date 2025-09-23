from functools import singledispatchmethod
from typing import List, Set, cast

from clingo import ast
from clingo.core import Library
from clingo.symbol import SymbolType

from fasp.ast.syntax_checking import SymbolSignature
from fasp.util.ast import (
    AST,
    ArgumentAST,
    FreshVariableGenerator,
    StatementAST,
    TermAST,
)


class UnnestFunctionsTransformer:
    """
    Recursively unnest evaluable functions in Clingo AST.
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
        used_variable_names: Set[str],
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.var_gen = FreshVariableGenerator(used_variable_names)
        self.unnested_functions: List[ast.LiteralComparison] = []
        # Memoization cache to avoid duplicate unnested variables/comparisons
        # Checks if same function with same args has already been unnested
        # Also checks for same TermSymbolic in the rule.
        self._cache: dict[tuple, TermAST] = {}
        # Map from variable name to the corresponding comparisons for lookup during rewrite
        self._var_to_comp: dict[str, ast.LiteralComparison] = {}

    def _is_evaluable(self, name: str, arity: int) -> bool:
        return SymbolSignature(name, arity) in self.evaluable_functions

    def _make_comparison(
        self, loc, left: TermAST, right: TermAST
    ) -> ast.LiteralComparison:
        return ast.LiteralComparison(
            self.lib,
            loc,
            ast.Sign.NoSign,
            cast(TermAST, left),  # type narrowing
            [ast.RightGuard(self.lib, ast.Relation.Equal, cast(TermAST, right))],
        )

    @singledispatchmethod
    def _unnest(self, node: AST, outer: bool = False) -> AST:
        """Default: recurse if possible, else return as-is."""
        # if hasattr(node, "transform"):
        return node.transform(self.lib, lambda c: self._unnest(c, outer)) or node
        # return node

    # Normalize compariosns to have evaluable functions on the left side of equality only
    @_unnest.register
    def _(self, node: ast.LiteralComparison, outer: bool = True) -> AST:

        if len(node.right) == 1 and node.right[0].relation != ast.Relation.Equal:
            outer = False

        new_left = cast(
            TermAST, self._unnest(node.left, outer)
        )  # False if not = and len(node.right) == 1
        new_right = [
            ast.RightGuard(
                self.lib,
                rg.relation,
                cast(TermAST, self._unnest(rg.term, outer=False)),
            )
            for rg in node.right
        ]
        return node.update(self.lib, left=new_left, right=new_right)

    # Need to pass outer=False to make sure TermFunctions and TermSymbolic functions in body aggregates are unnested
    # @_unnest.register
    # def _(self, node: ast.HeadAggregateElement, outer: bool = False) -> AST:
    #     return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node

    @_unnest.register
    def _(self, node: ast.BodyAggregateElement, outer: bool = False) -> AST:
        return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node

    @_unnest.register
    def _(self, node: ast.TermFunction, outer: bool = False) -> AST:
        new_pool = []
        for tup in node.pool:
            new_args: List[TermAST] = [
                cast(TermAST, self._unnest(t, outer=False)) for t in tup.arguments
            ]
            new_pool.append(ast.ArgumentTuple(self.lib, tuple(new_args)))
        new_func = node.update(self.lib, pool=tuple(new_pool))

        # Unnest if evaluable
        if not outer and self._is_evaluable(node.name, len(new_pool[0].arguments)):
            # normalize key by node name + args stringified
            key = (node.name, tuple(str(arg) for arg in new_pool[0].arguments))
            if key in self._cache:
                return self._cache[key]

            fresh: TermAST = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
            self._cache[key] = fresh
            comp = self._make_comparison(node.location, cast(TermAST, new_func), fresh)
            self.unnested_functions.append(comp)

            self._var_to_comp[cast(ast.TermFunction, fresh).name] = comp

            return fresh
        return new_func

    @_unnest.register
    def _(self, node: ast.TermSymbolic, outer: bool = False) -> AST:
        if node.symbol.type == SymbolType.Function:
            name = str(node.symbol.name)
            arity = len(node.symbol.arguments)
            if not outer and self._is_evaluable(name, arity):
                # normalize key by symbol name + args stringified
                key = (name, tuple(str(arg) for arg in node.symbol.arguments))
                if key in self._cache:
                    return self._cache[key]

                fresh = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
                self._cache[key] = fresh
                comp = self._make_comparison(node.location, node, fresh)
                self.unnested_functions.append(comp)
                self._var_to_comp[fresh.name] = comp
                return fresh
        return node

    def transform_rule(self, st: StatementAST) -> StatementAST:
        """Transform a single rule statement."""
        return cast(StatementAST, self._unnest(st, outer=True))
