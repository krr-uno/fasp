from functools import singledispatchmethod
from typing import List, Set, cast

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
from fasp.util.ast import (
    AST,
    FreshVariableGenerator,
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
        self._cache: dict[tuple[str, tuple[str, ...]], TermAST] = {}
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
            left,  # type narrowing
            [ast.RightGuard(self.lib, ast.Relation.Equal, right)],
        )

    def transform_rule(self, st: FASP_Statement) -> FASP_Statement:
        """Transform a single rule statement."""
        return cast(FASP_Statement, self._unnest(st, outer=True))

    @singledispatchmethod
    def _unnest(self, node: FASP_AST, outer: bool = False) -> FASP_AST:
        """Default: recurse if possible, else return as-is."""
        if hasattr(node, "transform"):
            return node.transform(self.lib, lambda c: self._unnest(c, outer)) or node
        return node

    @_unnest.register
    def _(self, node: AssignmentAST, outer: bool = False) -> AssignmentAST:
        return node.transform(self.lib, lambda lib, c: self._unnest(c, outer)) or node

    # Normalize compariosns to have evaluable functions on the left side of equality only
    @_unnest.register
    def _(
        self, node: ast.LiteralComparison, outer: bool = True
    ) -> AST:  # Should this be FASP_AST?

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

    # Need to pass outer=False to make sure TermFunctions and TermSymbolic functions in body aggregates are unnested
    # @_unnest.register
    # def _(
    #     self, node: ast.HeadAggregateElement, outer: bool = False
    # ) -> AST:  # Should this be FASP_AST?
    #     return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node

    @_unnest.register
    def _(
        self, node: ast.BodyAggregateElement, outer: bool = False
    ) -> AST:  # Should this be FASP_AST?
        return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node

    # NOTE: For test `test_assignment_with_aggregate` in tests\ast\rewriting\test_unnesting.py
    @_unnest.register
    def _(self, node: HeadAggregateAssignment, outer: bool = False) -> FASP_AST:
        """
        Handle head aggregates like:
            score(X) := #sum{ f(Y) : p(Y), q(X) }.
        We keep f(Y) (the term before ':') with outer=True
        and traverse conditions (after ':') with outer=False.
        """
        # Unnest the assigned function
        new_assigned = cast(
            ast.TermFunction, self._unnest(node.assigned_function, outer=True)
        )

        # aggregate_function is an enum, remains unchanged in unnest because it lacks "transform"
        new_agg_func = self._unnest(node.aggregate_function, outer=True)

        # Rebuild elements
        new_elements = []
        for elem in node.elements:
            # The first tuple is visited with outer=True
            new_tuple = [self._unnest(t, outer=True) for t in elem.tuple]
            # The condition literals are visited with outer=False
            new_condition = [self._unnest(c, outer=False) for c in elem.condition]

            new_elem = elem.update(
                self.lib,
                tuple=new_tuple,
                condition=new_condition,
            )
            new_elements.append(new_elem)

        return node.update(
            assigned_function=new_assigned,
            aggregate_function=new_agg_func,
            elements=new_elements,
        )

    @_unnest.register
    def _(
        self, node: ast.BodyAggregate, outer: bool = False
    ) -> AST:  # Should this be FASP_AST?
        # Visit left guard, elements, and right guard with outer=False
        return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node

    @_unnest.register
    def _(
        self, node: ast.TermFunction, outer: bool = False
    ) -> AST:  # Should this be FASP_AST?
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
    def _(
        self, node: ast.TermSymbolic, outer: bool = False
    ) -> AST:  # Should this be FASP_AST?
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


# With Debug Prints
# from functools import singledispatchmethod
# from typing import List, Set, cast

# from clingo import ast
# from clingo.core import Library, Location
# from clingo.symbol import SymbolType

# from fasp.ast._nodes import (
#     AssignmentAST,
#     FASP_Statement,
# )
# from fasp.ast.collectors import SymbolSignature
# from fasp.util.ast import (
#     AST,
#     FreshVariableGenerator,
#     TermAST,
# )


# class UnnestFunctionsTransformer:
#     """
#     Recursively unnest evaluable functions in Clingo AST.
#     """

#     def __init__(
#         self,
#         lib: Library,
#         evaluable_functions: Set[SymbolSignature],
#         used_variable_names: Set[str],
#     ):
#         self.lib = lib
#         self.evaluable_functions = evaluable_functions
#         self.var_gen = FreshVariableGenerator(used_variable_names)
#         self.unnested_functions: List[ast.LiteralComparison] = []
#         self._cache: dict[tuple[str, tuple[str, ...]], TermAST] = {}
#         self._var_to_comp: dict[str, ast.LiteralComparison] = {}

#     def _is_evaluable(self, name: str, arity: int) -> bool:
#         return SymbolSignature(name, arity) in self.evaluable_functions

#     def _is_evaluable_term(self, term: TermAST) -> bool:
#         if isinstance(term, ast.TermFunction):
#             return self._is_evaluable(term.name, len(term.pool[0].arguments))
#         if (
#             isinstance(term, ast.TermSymbolic)
#             and term.symbol.type == SymbolType.Function
#         ):
#             return self._is_evaluable(str(term.symbol.name), len(term.symbol.arguments))
#         return False

#     def _make_comparison(
#         self, loc: Location, left: TermAST, right: TermAST
#     ) -> ast.LiteralComparison:
#         return ast.LiteralComparison(
#             self.lib,
#             loc,
#             ast.Sign.NoSign,
#             left,
#             [ast.RightGuard(self.lib, ast.Relation.Equal, right)],
#         )

#     def transform_rule(self, st: FASP_Statement) -> FASP_Statement:
#         return cast(FASP_Statement, self._unnest(st, outer=True))

#     @singledispatchmethod
#     def _unnest(self, node: AST, outer: bool = False) -> AST:
#         print(f"[UNNEST] enter_default: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         result = node.transform(self.lib, lambda c: self._unnest(c, outer)) or node
#         print(f"[UNNEST] exit_default: node_type={type(node).__name__}, outer={outer}, node='{result}'")
#         return result

#     @_unnest.register
#     def _(self, node: AssignmentAST, outer: bool = False) -> AssignmentAST:
#         print(f"[UNNEST] enter_AssignmentAST: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         result = node.transform(self.lib, lambda lib, c: self._unnest(c, outer)) or node
#         print(f"[UNNEST] exit_AssignmentAST: node_type={type(node).__name__}, outer={outer}, node='{result}'")
#         return result

#     @_unnest.register
#     def _(self, node: ast.LiteralComparison, outer: bool = True) -> AST:
#         print(f"[UNNEST] enter_LiteralComparison: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         if len(node.right) == 1 and node.right[0].relation != ast.Relation.Equal:
#             outer = False
#         if len(node.right) == 1 and node.right[0].relation == ast.Relation.Equal:
#             left_eval = self._is_evaluable_term(node.left)
#             right_eval = self._is_evaluable_term(node.right[0].term)
#             print(f"[UNNEST] LiteralComparison equality: left_eval={left_eval}, right_eval={right_eval}")
#             if not left_eval and right_eval:
#                 new_left = cast(TermAST, self._unnest(node.right[0].term, outer))
#                 new_right = [
#                     ast.RightGuard(
#                         self.lib,
#                         ast.Relation.Equal,
#                         cast(TermAST, self._unnest(node.left, outer=False)),
#                     )
#                 ]
#                 result = node.update(self.lib, left=new_left, right=new_right)
#                 print(f"[UNNEST] exit_LiteralComparison_flipped: node='{result}'")
#                 return result
#         new_left = cast(TermAST, self._unnest(node.left, outer))
#         new_right = [
#             ast.RightGuard(
#                 self.lib,
#                 rg.relation,
#                 cast(TermAST, self._unnest(rg.term, outer=False)),
#             )
#             for rg in node.right
#         ]
#         result = node.update(self.lib, left=new_left, right=new_right)
#         print(f"[UNNEST] exit_LiteralComparison: node_type={type(node).__name__}, outer={outer}, node='{result}'")
#         return result

#     @_unnest.register
#     def _(self, node: ast.BodyAggregateElement, outer: bool = False) -> AST:
#         print(f"[UNNEST] enter_BodyAggregateElement: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         result = node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node
#         print(f"[UNNEST] exit_BodyAggregateElement: node_type={type(node).__name__}, outer={outer}, node='{result}'")
#         return result

#     @_unnest.register
#     def _(self, node: ast.BodyAggregate, outer: bool = False) -> AST:
#         print(f"[UNNEST] enter_BodyAggregate: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         result = node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node
#         print(f"[UNNEST] exit_BodyAggregate: node_type={type(node).__name__}, outer={outer}, node='{result}'")
#         return result

#     @_unnest.register
#     def _(self, node: ast.TermFunction, outer: bool = False) -> AST:
#         print(f"[UNNEST] enter_TermFunction: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         new_pool = []
#         for tup in node.pool:
#             new_args: List[TermAST] = [
#                 cast(TermAST, self._unnest(t, outer=False)) for t in tup.arguments
#             ]
#             new_pool.append(ast.ArgumentTuple(self.lib, tuple(new_args)))
#         new_func = node.update(self.lib, pool=tuple(new_pool))
#         if not outer and self._is_evaluable(node.name, len(new_pool[0].arguments)):
#             key = (node.name, tuple(str(arg) for arg in new_pool[0].arguments))
#             if key in self._cache:
#                 print(f"[UNNEST] exit_TermFunction_cached: node='{self._cache[key]}'")
#                 return self._cache[key]
#             fresh: TermAST = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
#             self._cache[key] = fresh
#             comp = self._make_comparison(node.location, cast(TermAST, new_func), fresh)
#             self.unnested_functions.append(comp)
#             self._var_to_comp[cast(ast.TermFunction, fresh).name] = comp
#             print(f"[UNNEST] exit_TermFunction_unnested: node='{fresh}', comparison='{comp}'")
#             return fresh
#         print(f"[UNNEST] exit_TermFunction_no_unnest: node='{new_func}'")
#         return new_func

#     @_unnest.register
#     def _(self, node: ast.TermSymbolic, outer: bool = False) -> AST:
#         print(f"[UNNEST] enter_TermSymbolic: node_type={type(node).__name__}, outer={outer}, node='{node}'")
#         if node.symbol.type == SymbolType.Function:
#             name = str(node.symbol.name)
#             arity = len(node.symbol.arguments)
#             if not outer and self._is_evaluable(name, arity):
#                 key = (name, tuple(str(arg) for arg in node.symbol.arguments))
#                 if key in self._cache:
#                     print(f"[UNNEST] exit_TermSymbolic_cached: node='{self._cache[key]}'")
#                     return self._cache[key]
#                 fresh = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
#                 self._cache[key] = fresh
#                 comp = self._make_comparison(node.location, node, fresh)
#                 self.unnested_functions.append(comp)
#                 self._var_to_comp[fresh.name] = comp
#                 print(f"[UNNEST] exit_TermSymbolic_unnested: node='{fresh}', comparison='{comp}'")
#                 return fresh
#         print(f"[UNNEST] exit_TermSymbolic_no_unnest: node='{node}'")
#         return node
