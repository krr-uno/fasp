from functools import singledispatchmethod
from typing import List, Set, Tuple, cast

from clingo import ast
from clingo.core import Library, Location
from clingo.symbol import SymbolType

from fasp.ast._nodes import (
    FASP_AST,
    AssignmentAST,
    HeadAggregateAssignment,
    HeadSimpleAssignment,
)
from fasp.ast.collectors import SymbolSignature
from fasp.util.ast import AST, AST_T, FreshVariableGenerator, TermAST


def unnest_functions(
    lib: Library,
    node: FASP_AST,
    evaluable_functions: Set[SymbolSignature],
    variable_generator: FreshVariableGenerator,
    *,
    outer: bool = True,
    sign: ast.Sign | None = None,
    is_in_head: bool = False,
    allow_evaluable_in_negative_literal: bool = True,
    # Might need to pass flag boolens like outer (already used downstream) and allow_evaluable_in_negative_literal (new)
) -> tuple[FASP_AST, List[ast.LiteralComparison]]:
    """
    Unnest evaluable functions in a given rule and return the list of generated comparisons.
    """
    transformer = UnnestFunctionsTransformer(
        lib, evaluable_functions, variable_generator=variable_generator
    )

    return transformer.transform_node(node, outer, sign, is_in_head)


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
        is_in_head: bool = False,
        allow_evaluable_in_negative_literal: bool = True,
    ) -> Tuple[FASP_AST, List[ast.LiteralComparison]]:
        """
        Transform one AST node and return (new_node, unnested_list).

        - Resets the per-node list `self.unnested_functions` before transformation.
        - Keeps the same FreshVariableGenerator instance so successive calls across
          nodes in the same rule reuse the same variable namespace.
        """
        # Clear per-node collected unnested comparisons
        self.unnested_functions = []

        new_node = self._unnest(node, outer=outer, sign=None, is_in_head=is_in_head)

        # Copy the list and return
        collected = list(self.unnested_functions)
        return new_node, collected

    # def transform_rule(self, st: FASP_Statement) -> FASP_Statement:
    #     """Transform a single rule statement."""
    #     return cast(FASP_Statement, self._unnest(st, outer=True))

    @singledispatchmethod
    def _unnest(
        self,
        node: FASP_AST,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> FASP_AST:
        """Default: recurse if possible, else return as-is."""
        return node.transform(self.lib, self._unnest, outer, sign, is_in_head) or node

    @_unnest.register
    def _(
        self,
        node: AssignmentAST,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> AssignmentAST:
        return node.transform(self.lib, self._unnest, outer, sign, is_in_head) or node

    @_unnest.register
    def _(
        self,
        node: HeadSimpleAssignment,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> HeadSimpleAssignment:
        new_assigned = self._unnest(
            node.assigned_function, outer, sign, is_in_head=is_in_head
        )
        new_value = self._unnest(
            node.value, outer=False, sign=sign, is_in_head=is_in_head
        )
        return node.update(self.lib, assigned_function=new_assigned, value=new_value)
        # return node.transform(self.lib, self._unnest, outer) or node

    # Normalize compariosns to have evaluable functions on the left side of equality only
    @_unnest.register
    def _(
        self,
        node: ast.LiteralComparison,
        outer: bool = True,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.LiteralComparison:
        # print(
        #     f"Unnesting LiteralComparison: {str(node)}, is_in_head={is_in_head}, outer={outer}, sign={sign}"
        # )
        if len(node.right) == 1 and node.right[0].relation != ast.Relation.Equal:
            outer = False

        if isinstance(node.left, ast.TermBinaryOperation):
            outer = False

        # Special case: equality with evaluable only on right-hand side
        if len(node.right) == 1 and node.right[0].relation == ast.Relation.Equal:
            left_eval = self._is_evaluable_term(node.left)
            right_eval = self._is_evaluable_term(node.right[0].term)

            if not left_eval and right_eval:
                # Flip sides instead of unnesting into a fresh var
                new_left = cast(
                    TermAST,
                    self._unnest(
                        node.right[0].term, outer, sign=sign, is_in_head=is_in_head
                    ),
                )
                new_right = [
                    ast.RightGuard(
                        self.lib,
                        ast.Relation.Equal,
                        cast(
                            TermAST,
                            self._unnest(
                                node.left, outer=False, sign=sign, is_in_head=is_in_head
                            ),
                        ),
                    )
                ]
                return node.update(self.lib, left=new_left, right=new_right)

        # Unnest left with outer = False if the comparison is an Equality
        # See test: test_comparison_with_equality_in_head
        # print(
        #     f"Unnesting node {str(node)} right {str(node.right)} left {str(node.left)}"
        # )
        _outer = outer
        if (
            len(node.right) == 1
            and node.right[0].relation == ast.Relation.Equal
            and self._is_evaluable_term(node.left)
            and is_in_head
        ):
            _outer = False
        new_left = cast(
            TermAST, self._unnest(node.left, _outer, sign=sign, is_in_head=is_in_head)
        )  # False if not = and len(node.right) == 1
        new_right = [
            ast.RightGuard(
                self.lib,
                rg.relation,
                cast(
                    TermAST,
                    self._unnest(
                        rg.term, outer=False, sign=sign, is_in_head=is_in_head
                    ),
                ),
            )
            for rg in node.right
        ]
        return node.update(
            self.lib,
            left=new_left,
            right=new_right,
            sign=node.sign if sign is None else sign,
        )

    @_unnest.register
    def _(
        self,
        node: ast.BodyAggregate | ast.HeadAggregate,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.BodyAggregate | ast.HeadAggregate:
        return (
            node.transform(
                self.lib,
                lambda c: self._unnest(
                    c, outer=False, sign=sign, is_in_head=is_in_head
                ),
            )
            or node
        )

    @_unnest.register
    def _(
        self,
        node: ast.RightGuard | ast.LeftGuard,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.RightGuard | ast.LeftGuard:
        return (
            node.transform(
                self.lib,
                lambda t: self._unnest(
                    t, outer=False, sign=sign, is_in_head=is_in_head
                ),
            )
            or node
        )

    @_unnest.register
    def _(
        self,
        node: ast.BodyAggregateElement | ast.HeadAggregateElement,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.BodyAggregateElement | ast.HeadAggregateElement:
        """
        Handle elements of aggregates of the form:
            #sum{ a(X) : p(f(X)), q(X) }
        Also handles head aggregates

        The tuple (a(X)) is treated as *inner* (unnested),
        while the condition (p(f(X)), q(X)) is evaluated as *outer*
        — because predicates should not be unnested in conditions.
        The literal in head aggregates is also treated as outer.
        """
        print(f"Unnesting Aggregate {str(node)} {type(node)}")
        # Unnest tuple terms immediately (inner context)
        new_tuple = [
            self._unnest(t, outer=False, sign=sign, is_in_head=is_in_head)
            for t in node.tuple
        ]

        # Traverse condition literals as outer (no unnesting of predicate calls)
        # new_condition = [
        #     self._unnest(c, outer=True, sign=sign, is_in_head=is_in_head)
        #     for c in node.condition
        # ]

        new_condition = []
        for cond in node.condition:
            new_c = self._unnest(cond, outer=True, sign=sign, is_in_head=is_in_head)
            # NOTE: Disallow negation with evaluable in aggregate
            if new_c != cond and cond.sign == ast.Sign.Single:
                raise RuntimeError(
                    f"Negation is not supported with evaluable functions in Aggregate. Found {str(cond)} at {cond.location}."
                )
            new_condition.append(new_c)

        if isinstance(node, ast.HeadAggregateElement):
            new_literal = self._unnest(
                node.literal, outer=True, sign=sign, is_in_head=is_in_head
            )
            # NOTE: Disallow negation with evaluable in aggregate
            if new_literal != node.literal and node.literal.sign == ast.Sign.Single:
                raise RuntimeError(
                    f"Negation is not supported with evaluable functions in Aggregate. Found {str(node.literal)} at {node.literal.location}."
                )
            # else:
            return node.update(
                self.lib,
                literal=new_literal,
                tuple=new_tuple,
                condition=new_condition,
            )
        return node.update(self.lib, tuple=new_tuple, condition=new_condition)

    # Passing down the sign of the literal to TermFunction/TermSymbolic dispatchers for handling double negation
    @_unnest.register
    def _(
        self,
        node: ast.BodySimpleLiteral | ast.HeadSimpleLiteral,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.BodySimpleLiteral | ast.HeadSimpleLiteral:
        # Pass current node.sign downward for inner literals
        if isinstance(node, ast.HeadSimpleLiteral):
            is_in_head = True
        new_literal = self._unnest(
            node.literal, outer=outer, sign=node.literal.sign, is_in_head=is_in_head
        )
        return node.update(self.lib, literal=new_literal)

    # Need to pass outer=False to make sure TermFunctions and TermSymbolic functions in body aggregates are unnested
    # @_unnest.register
    # def _(
    #     self, node: ast.HeadAggregateElement, outer: bool = False
    # ) -> ast.HeadAggregateElement:
    #     # Unnest tuple terms immediately (inner context)
    #     new_tuple = [self._unnest(t, outer=False) for t in node.tuple]
    #     new_condition = [self._unnest(c, outer=True) for c in node.condition]
    #     new_literal = self._unnest(node.literal, outer=True)
    #     return node.update(
    #         self.lib, literal=new_literal, tuple=new_tuple, condition=new_condition
    #     )

    # NOTE: For test `test_assignment_with_aggregate` in tests\ast\rewriting\test_unnesting.py
    @_unnest.register
    def _(
        self,
        node: HeadAggregateAssignment,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> HeadAggregateAssignment:
        """
        Handle head aggregates like:
            score(X) := #sum{ f(Y) : p(Y), q(X) }.
        We keep f(Y) (the term before ':') with outer=True since it is in the head of the rule.
        and traverse conditions (after ':') with outer=True? or False? Need to confirm.
        """

        # Unnest the assigned function
        new_assigned = cast(
            ast.TermFunction,
            self._unnest(node.assigned_function, outer=True, sign=sign),
        )

        # Rebuild elements
        new_elements = []
        for elem in node.elements:
            # The first tuple is visited with outer=True
            new_tuple = [
                self._unnest(t, outer=True, sign=sign, is_in_head=is_in_head)
                for t in elem.tuple
            ]
            # The condition literals are visited with outer=True
            new_condition = [
                self._unnest(c, outer=True, sign=sign, is_in_head=is_in_head)
                for c in elem.condition
            ]

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

    # NOTE: Disallow negation with evaluable in BodyConditionalLiteral
    @_unnest.register
    def _(
        self,
        node: ast.BodyConditionalLiteral,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.BodyConditionalLiteral:
        # return node.transform(self.lib, self._unnest, outer, sign, is_in_head) or node

        new_literal = self._unnest(node.literal, outer, sign, is_in_head)
        new_condition = []
        for cond in node.condition:
            new_c = self._unnest(cond, outer, sign, is_in_head)
            if new_c != cond and cond.sign == ast.Sign.Single:
                raise RuntimeError(
                    f"Negation is not supported with evaluable functions in Body Conditional Literal. Found {str(cond)} at {cond.location}."
                )
            new_condition.append(new_c)
        return node.update(self.lib, literal=new_literal, condition=new_condition)

    @_unnest.register
    def _(
        self,
        node: ast.TermFunction,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.TermFunction:
        new_pool = []
        for tup in node.pool:
            new_args: List[TermAST] = [
                cast(
                    TermAST,
                    self._unnest(t, outer=False, sign=sign, is_in_head=is_in_head),
                )
                for t in tup.arguments
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
            comp = self._make_comparison(
                node.location, cast(TermAST, new_func), fresh, sign=sign
            )
            self.unnested_functions.append(comp)
            fresh = cast(ast.TermFunction, fresh)

            return fresh
        return new_func

    @_unnest.register
    def _(
        self,
        node: ast.TermSymbolic,
        outer: bool = False,
        sign: ast.Sign | None = None,
        is_in_head: bool = False,
    ) -> ast.TermSymbolic:
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
                comp = self._make_comparison(node.location, node, fresh, sign=sign)
                self.unnested_functions.append(comp)

                fresh = cast(ast.TermSymbolic, fresh)
                return fresh
        return node
