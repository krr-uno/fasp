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
        print(f"Unnesting node type: {type(node)}")
        # if hasattr(node, "transform"):
        return node.transform(self.lib, lambda c: self._unnest(c, outer)) or node
        # return node

    @_unnest.register
    def _(self, node: ast.LiteralComparison, outer: bool = False) -> AST:
        new_left = cast(TermAST, self._unnest(node.left, outer=True)) # True if not = and len(node.right) == 1
        new_right = [
            ast.RightGuard(
                self.lib,
                rg.relation,
                cast(TermAST, self._unnest(rg.term, outer=True)),
            )
            for rg in node.right
        ]
        return node.update(self.lib, left=new_left, right=new_right)

    @_unnest.register
    def _(self, node: ast.TermFunction, outer: bool = False) -> AST:
        print(f"Unnesting node type: {type(node)}")
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
                return fresh
        return node

    def transform_rule(self, st: StatementAST) -> StatementAST:
        """Transform a single rule statement."""
        return cast(StatementAST, self._unnest(st, outer=True))

    def rewrite_rule_with_unnested(
        self, st: StatementAST
    ) -> tuple[StatementAST, List[ast.LiteralComparison]]:
        """
        Transform a single rule and return (rewritten_rule_with_unnested_in_body, unnested_comparisons).

        - resets self.unnested_functions before processing the rule
        - runs the existing transform_rule
        - if the result is a StatementRule, appends the collected unnested comparisons
          (wrapped as BodySimpleLiteral) to the rule body via .update(...)
        - returns the final StatementRule and the list of collected LiteralComparison objects
        """
        # clear previous state
        self.unnested_functions = []
        self._cache = {}  # clear cache
        self.var_gen = FreshVariableGenerator(set())

        new_st = self.transform_rule(st)

        # if it's a rule statement, append the collected comparisons to the body
        if isinstance(new_st, ast.StatementRule):
            # existing body (sequence) -> list
            existing_body = list(new_st.body)
            # wrap each LiteralComparison as a BodySimpleLiteral
            extra_body = [
                ast.BodySimpleLiteral(self.lib, comp)
                for comp in self.unnested_functions
            ]
            new_body = existing_body + extra_body
            # produce a new StatementRule with the extended body
            new_rule = new_st.update(self.lib, body=new_body)
            return new_rule, list(self.unnested_functions)

        # not a rule (no body to extend)
        return new_st, list(self.unnested_functions)

    def rewrite_rules_with_unnested(
        self, rules: List[StatementAST]
    ) -> List[tuple[StatementAST, List[ast.LiteralComparison]]]:
        """
        Transform a list of rules and return a list of tuples (rewritten_rule, unnested_comparisons).
        Use this when you want to rewrite an entire program (rule-by-rule).
        """
        out: List[tuple[StatementAST, List[ast.LiteralComparison]]] = []
        for r in rules:
            out.append(self.rewrite_rule_with_unnested(r))
        return out
