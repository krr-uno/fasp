from functools import singledispatchmethod
from typing import List, Set, Tuple, cast

from clingo import ast
from clingo.core import Library, Location
from clingo.symbol import SymbolType

from fasp.ast._nodes import (
    FASP_AST,
    AssignmentAST,
    FASP_Statement,
    HeadAggregateAssignment,
)
from fasp.ast.collectors import SymbolSignature
from fasp.util.ast import AST, AST_T, FreshVariableGenerator, TermAST


def unnest_functions(
    lib: Library,
    node: FASP_AST,
    evaluable_functions: Set[SymbolSignature],
    variable_generator: FreshVariableGenerator,
    outer: bool = True,
    # Might need to pass flag boolens like outer (already used downstream) and allow_evaluable_in_negative_literal (new)
) -> tuple[FASP_AST, List[ast.LiteralComparison]]:
    """
    Unnest evaluable functions in a given rule and return the list of generated comparisons.
    """
    transformer = UnnestFunctionsTransformer(
        lib, evaluable_functions, variable_generator=variable_generator
    )

    return transformer.transform_node(node, outer)


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
        # evaluable_functions_allowed_in_negated_literals: bool
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.var_gen = variable_generator
        self.unnested_functions: List[ast.LiteralComparison] = []

        # Memoization cache to avoid duplicate unnested variables/comparisons
        # Checks if same function with same args has already been unnested
        # Also checks for same TermSymbolic in the rule.
        # self._cache: dict[tuple[str, tuple[str, ...]], TermAST] = {}

        # Map from variable name to the corresponding comparisons for lookup during rewrite
        self._var_to_comp: dict[str, ast.LiteralComparison] = {}

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
        self, loc: Location, left: TermAST, right: TermAST
    ) -> ast.LiteralComparison:
        return ast.LiteralComparison(
            self.lib,
            loc,
            ast.Sign.NoSign,
            left,
            [ast.RightGuard(self.lib, ast.Relation.Equal, right)],
        )

    def transform_node(
        self, node: FASP_AST, outer: bool = True
    ) -> Tuple[FASP_AST, List[ast.LiteralComparison]]:
        """
        Transform one AST node and return (new_node, unnested_list).

        - Resets the per-node list `self.unnested_functions` before transformation.
        - Keeps the same FreshVariableGenerator instance so successive calls across
          nodes in the same rule reuse the same variable namespace.
        """
        # Clear per-node collected unnested comparisons
        self.unnested_functions = []

        new_node = self._unnest(node, outer=outer)

        # Copy the list and return
        collected = list(self.unnested_functions)
        return new_node, collected

    # def transform_rule(self, st: FASP_Statement) -> FASP_Statement:
    #     """Transform a single rule statement."""
    #     return cast(FASP_Statement, self._unnest(st, outer=True))

    @singledispatchmethod
    def _unnest(self, node: FASP_AST, outer: bool = False) -> FASP_AST:
        """Default: recurse if possible, else return as-is."""
        return node.transform(self.lib, self._unnest, outer) or node

    @_unnest.register
    def _(self, node: AssignmentAST, outer: bool = False) -> AssignmentAST:
        return node.transform(self.lib, self._unnest, outer) or node

    # Normalize compariosns to have evaluable functions on the left side of equality only
    @_unnest.register
    def _(
        self, node: ast.LiteralComparison, outer: bool = True
    ) -> ast.LiteralComparison:

        if len(node.right) == 1 and node.right[0].relation != ast.Relation.Equal:
            outer = False

        # Special case: equality with evaluable only on right-hand side
        if len(node.right) == 1 and node.right[0].relation == ast.Relation.Equal:
            left_eval = self._is_evaluable_term(node.left)
            right_eval = self._is_evaluable_term(node.right[0].term)

            if not left_eval and right_eval:
                # Flip sides instead of unnesting into a fresh var
                new_left = cast(TermAST, self._unnest(node.right[0].term, outer))
                new_right = [
                    ast.RightGuard(
                        self.lib,
                        ast.Relation.Equal,
                        cast(TermAST, self._unnest(node.left, outer=False)),
                    )
                ]
                return node.update(self.lib, left=new_left, right=new_right)
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

    @_unnest.register
    def _(
        self, node: ast.BodyAggregate | ast.HeadAggregate, outer: bool = False
    ) -> ast.BodyAggregate | ast.HeadAggregate:
        # Visit left guard, elements, and right guard with outer=False
        return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node

    @_unnest.register
    def _(
        self, node: ast.BodyAggregateElement, outer: bool = False
    ) -> ast.BodyAggregateElement:
        """
        Handle elements of body aggregates of the form:
            #sum{ a(X) : p(f(X)), q(X) }

        The tuple (a(X)) is treated as *inner* (unnested),
        while the condition (p(f(X)), q(X)) is evaluated as *outer*
        — because predicates should not be unnested in conditions.
        """
        # Unnest tuple terms immediately (inner context)
        new_tuple = [self._unnest(t, outer=False) for t in node.tuple]

        # Traverse condition literals as outer (no unnesting of predicate calls)
        new_condition = [self._unnest(c, outer=True) for c in node.condition]

        return node.update(self.lib, tuple=new_tuple, condition=new_condition)

    # Need to pass outer=False to make sure TermFunctions and TermSymbolic functions in body aggregates are unnested
    @_unnest.register
    def _(
        self, node: ast.HeadAggregateElement, outer: bool = False
    ) -> ast.HeadAggregateElement:
        # Unnest tuple terms immediately (inner context)
        new_tuple = [self._unnest(t, outer=False) for t in node.tuple]
        new_condition = [self._unnest(c, outer=True) for c in node.condition]
        new_literal = self._unnest(node.literal, outer=True)
        return node.update(
            self.lib, literal=new_literal, tuple=new_tuple, condition=new_condition
        )

    # NOTE: For test `test_assignment_with_aggregate` in tests\ast\rewriting\test_unnesting.py
    @_unnest.register
    def _(
        self, node: HeadAggregateAssignment, outer: bool = False
    ) -> HeadAggregateAssignment:
        """
        Handle head aggregates like:
            score(X) := #sum{ f(Y) : p(Y), q(X) }.
        We keep f(Y) (the term before ':') with outer=True since it is in the head of the rule.
        and traverse conditions (after ':') with outer=True? or False? Need to confirm.
        """

        # Unnest the assigned function
        new_assigned = cast(
            ast.TermFunction, self._unnest(node.assigned_function, outer=True)
        )

        # Rebuild elements
        new_elements = []
        for elem in node.elements:
            # The first tuple is visited with outer=True
            new_tuple = [self._unnest(t, outer=True) for t in elem.tuple]
            # The condition literals are visited with outer=True
            new_condition = [self._unnest(c, outer=True) for c in elem.condition]

            new_elem = elem.update(
                self.lib,
                tuple=new_tuple,
                condition=new_condition,
            )
            new_elements.append(new_elem)

        return node.update(
            assigned_function=new_assigned,
            elements=new_elements,
        )

    @_unnest.register
    def _(self, node: ast.TermFunction, outer: bool = False) -> ast.TermFunction:
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
            (node.name, tuple(str(arg) for arg in new_pool[0].arguments))
            # if key in self._cache:
            #     print(type(self._cache[key]))
            #     return self._cache[key]

            fresh: TermAST = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
            # self._cache[key] = fresh
            comp = self._make_comparison(node.location, cast(TermAST, new_func), fresh)
            self.unnested_functions.append(comp)
            fresh = cast(ast.TermFunction, fresh)
            self._var_to_comp[fresh.name] = comp

            return fresh
        return new_func

    @_unnest.register
    def _(self, node: ast.TermSymbolic, outer: bool = False) -> ast.TermSymbolic:
        if node.symbol.type == SymbolType.Function:
            name = str(node.symbol.name)
            arity = len(node.symbol.arguments)
            if not outer and self._is_evaluable(name, arity):
                # # normalize key by symbol name + args stringified
                # key = (name, tuple(str(arg) for arg in node.symbol.arguments))
                # if key in self._cache:
                #     return self._cache[key]

                fresh = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
                # self._cache[key] = fresh
                comp = self._make_comparison(node.location, node, fresh)
                self.unnested_functions.append(comp)

                self._var_to_comp[fresh.name] = comp
                fresh = cast(ast.TermSymbolic, fresh)
                return fresh
        return node
