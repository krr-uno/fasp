from functools import singledispatchmethod
from typing import List, Set, Tuple, cast

from clingo import ast
from clingo.core import Library, Location
from clingo.symbol import SymbolType

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
    node: FASP_AST,
    evaluable_functions: Set[SymbolSignature],
    variable_generator: FreshVariableGenerator,
    *,
    outer: bool = True,
    sign: ast.Sign | None = None,
    unnest_left_guard_equality: bool = False,
    allowed_in_negated_literals: bool = True,
    # Might need to pass flag boolens like outer (already used downstream) and allow_evaluable_in_negative_literal (new)
) -> tuple[FASP_AST, List[ast.LiteralComparison]]:
    """
    Unnest evaluable functions in a given rule and return the list of generated comparisons.
    """
    transformer = UnnestFunctionsTransformer(
        lib,
        evaluable_functions,
        variable_generator,
        unnest_left_guard_equality,
        allowed_in_negated_literals,
    )

    return transformer.transform_node(
        node,
        outer,
        sign,
    )


class UnnestFunctionsTransformer:
    """
    Recursively unnest evaluable functions in Clingo AST.
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
        # used_variable_names: Set[str],
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

        # Memoization cache to avoid duplicate unnested variables/comparisons
        # Checks if same function with same args has already been unnested
        # Also checks for same TermSymbolic in the rule.
        # self._cache: dict[tuple[str, tuple[str, ...]], TermAST] = {}

    def _is_evaluable(self, name: str, arity: int) -> bool:
        return SymbolSignature(name, arity) in self.evaluable_functions

    def _is_evaluable_term(self, term: TermAST) -> bool:
        if isinstance(term, ast.TermFunction):
            return self._is_evaluable(term.name, len(term.pool[0].arguments))
        if (
            isinstance(term, ast.TermSymbolic)
            and term.symbol.type == SymbolType.Function
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

        new_node = self._unnest(
            node,
            outer=outer,
            sign=None,
        )

        # Copy the list and return
        collected = list(self.unnested_functions)
        return new_node, collected

    @singledispatchmethod
    def _unnest(
        self,
        node: FASP_AST_T,
        outer: bool = False,
        sign: ast.Sign | None = None,
    ) -> FASP_AST_T:
        """
        Unnest evaluable functions in the given AST node.
        """
        return (
            node.transform(
                self.lib,
                self._unnest,
                outer,
                sign,
            )
            or node
        )

    @_unnest.register
    def _(
        self,
        node: HeadSimpleAssignment,
        outer: bool = False,
        sign: ast.Sign | None = None,
    ) -> HeadSimpleAssignment:
        new_assigned = self._unnest(
            node.assigned_function,
            outer,
            sign,
        )
        new_value = self._unnest(
            node.value,
            outer=False,
            sign=sign,
        )
        return node.update(self.lib, assigned_function=new_assigned, value=new_value)

    @_unnest.register
    def _(
        self,
        node: ast.LiteralSymbolic,
        outer: bool = True,
        _: ast.Sign | None = None,
    ) -> ast.LiteralSymbolic:
        return node.transform(self.lib, self._unnest, outer, node.sign) or node

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
        _: bool = True,
        sign: ast.Sign | None = None,
    ) -> ast.LiteralComparison:
        """
        Normalize comparisons to have evaluable functions on the left side of equality only
        """
        outer = False
        # Special case: equality with a single right guard
        if len(node.right) == 1 and node.right[0].relation == ast.Relation.Equal:
            # Flip if evaluable only on right-hand side
            if not self._is_evaluable_term(node.left) and self._is_evaluable_term(
                node.right[0].term
            ):
                node = self._flip_equality(node)

            if not self.unnest_left_guard_equality:
                outer = True

        new_left = self._unnest(node.left, outer, sign=sign)
        new_right = [
            rg.update(self.lib, term=self._unnest(rg.term, outer=False, sign=sign))
            for rg in node.right
        ]
        return node.update(
            self.lib,
            left=new_left,
            right=new_right,
        )

    @_unnest.register
    def _(
        self,
        node: ast.TermFunction | ast.TermSymbolic,
        outer: bool = False,
        sign: ast.Sign | None = None,
    ) -> ast.TermFunction | ast.TermSymbolic | ast.TermVariable:
        if isinstance(node, ast.TermFunction):
            pool = []
            for tup in node.pool:
                new_args: list[TermAST] = [
                    self._unnest(t, outer=False, sign=sign) for t in tup.arguments
                ]
                pool.append(ast.ArgumentTuple(self.lib, new_args))
            node = node.update(self.lib, pool=tuple(pool))
            name = node.name
            arguments = node.pool[0].arguments
        elif node.symbol.type != SymbolType.Function:
            return node
        else:
            new_args: list[TermAST] = [
                self._unnest(t, outer=False, sign=sign) for t in node.symbol.arguments
            ]
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
        return node

   