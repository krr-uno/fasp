from functools import singledispatchmethod
from typing import Any, List, Set, cast

from clingo import ast
from clingo.core import Library
from clingo.symbol import SymbolType


from fasp.ast.syntax_checking import (
    SymbolSignature
)
from fasp.util.ast import (
    AST,
    TermAST,
    ArgumentAST,
    FreshVariableGenerator,
    StatementAST,
    TermAST
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

    def _is_evaluable(self, name: str, arity: int) -> bool:
        return SymbolSignature(name, arity) in self.evaluable_functions

    def _make_comparison(self, loc, left: TermAST, right: TermAST) -> ast.LiteralComparison:
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
        return node.transform(
                self.lib, lambda c: self._unnest(c, outer=False)
            ) or node
        # return node

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
        if self._is_evaluable(node.name, len(new_pool[0].arguments)):
            fresh: TermAST = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
            comp = self._make_comparison(node.location, cast(TermAST, new_func), fresh)
            self.unnested_functions.append(comp)
            return fresh
        return new_func

    @_unnest.register
    def _(self, node: ast.TermSymbolic, outer: bool = False) -> AST:
        if node.symbol.type == SymbolType.Function:
            name = str(node.symbol.name)
            arity = len(node.symbol.arguments)
            if self._is_evaluable(name, arity):
                fresh = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
                comp = self._make_comparison(node.location, node, fresh)
                self.unnested_functions.append(comp)
                return fresh
        return node

    def transform_rule(self, st: StatementAST) -> StatementAST:
        """Transform a single rule statement."""
        return cast(StatementAST, self._unnest(st, outer=True))
    
    def rewrite_rule_with_unnested(self, st: StatementAST) -> tuple[StatementAST, List[ast.LiteralComparison]]:
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

        new_st = self.transform_rule(st)

        # if it's a rule statement, append the collected comparisons to the body
        if isinstance(new_st, ast.StatementRule):
            # existing body (sequence) -> list
            existing_body = list(new_st.body)
            # wrap each LiteralComparison as a BodySimpleLiteral
            extra_body = [ast.BodySimpleLiteral(self.lib, comp) for comp in self.unnested_functions]
            new_body = existing_body + extra_body
            # produce a new StatementRule with the extended body
            new_rule = new_st.update(self.lib, body=new_body)
            return new_rule, list(self.unnested_functions)

        # not a rule (no body to extend)
        return new_st, list(self.unnested_functions)

    def rewrite_rules_with_unnested(self, rules: List[StatementAST]) -> List[tuple[StatementAST, List[ast.LiteralComparison]]]:
        """
        Transform a list of rules and return a list of tuples (rewritten_rule, unnested_comparisons).
        Use this when you want to rewrite an entire program (rule-by-rule).
        """
        out: List[tuple[StatementAST, List[ast.LiteralComparison]]] = []
        for r in rules:
            out.append(self.rewrite_rule_with_unnested(r))
        return out


# class UnnestFunctionsTransformer:
#     """
#     Recursively unnest evaluable functions in Clingo AST.
#     """

#     def __init__(self, lib, evaluable_functions: Set[SymbolSignature], used_variable_names: Set[str]):
#         self.lib = lib
#         self.evaluable_functions = evaluable_functions
#         self.var_gen = FreshVariableGenerator(used_variable_names)
#         self.unnested_functions: List[ast.LiteralComparison] = []

#     def _is_evaluable(self, name: str, arity: int) -> bool:
#         return SymbolSignature(name, arity) in self.evaluable_functions

#     # ----------------- Transformer core -----------------

#     def _unnest(self, node: AST, outer: bool = False) -> AST:
#         """
#         Transformer callback passed to node.transform.
#         Special-case TermFunction, otherwise recurse normally.
#         """
#         if isinstance(node, ast.TermFunction):
#             # Recurse into arguments
#             new_pool = []
#             for tup in node.pool:
#                 new_args: List[TermAST] = [cast(TermAST, self._unnest(t, outer=False)) for t in tup.arguments]
#                 new_pool.append(ast.ArgumentTuple(self.lib, tuple(new_args)))
#             new_func = node.update(self.lib, pool=tuple(new_pool))

#             if self._is_evaluable(node.name, len(new_pool[0].arguments)):
#                 fresh = self.var_gen.fresh_variable(self.lib, node.location, "FUN")
#                 comp = ast.LiteralComparison(
#                     self.lib, node.location, ast.Sign.NoSign, new_func,
#                     [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)]
#                 )
#                 self.unnested_functions.append(comp)
#                 return fresh
#             return new_func

#         # Default: recurse
#         if hasattr(node, "transform"):
#             return node.transform(self.lib, lambda c: self._unnest(c, outer=False)) or node
#         return node


#     # ----------------- Entry points -----------------

#     def transform_rule(self, st: StatementAST) -> StatementAST:
#         # For top-level head/body terms we mark outer=True
#         return cast(StatementAST, self._unnest(st, outer=True))

#     # def transform_statements(self, statements: List[StatementAST]) -> List[StatementAST]:
#     #     return [self.transform_rule(st) for st in statements]
    


# class UnnestFunctionsTransformer:
#     """
#     Recursively unnest evaluable functions in Clingo 6 AST.
#     Fills `unnested_functions` with LiteralComparison objects of the form:
#         original_term = FUNx
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

#     def _is_evaluable(self, name: str, arity: int) -> bool:
#         return SymbolSignature(name, arity) in self.evaluable_functions

#     # ----------------- Term node handlers -----------------
#     @singledispatchmethod
#     def _unnest_term(self, term: ArgumentAST) -> ArgumentAST:
#         # fallback: return as-is
#         return term

#     # @_unnest_term.register
#     # def _(self, term: ast.TermVariable) -> ArgumentAST:
#     #     return term

#     @_unnest_term.register
#     def _(self, term: ast.TermSymbolic) -> ArgumentAST:
#         # Zero arity evaluable funcitons
#         if term.symbol.type == SymbolType.Function and self._is_evaluable(
#             term.symbol.name, 0
#         ):
#             fresh = self.var_gen.fresh_variable(self.lib, term.location, "FUN")
#             comp = ast.LiteralComparison(
#                 self.lib,
#                 term.location,
#                 ast.Sign.NoSign,
#                 term,
#                 [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
#             )
#             self.unnested_functions.append(comp)
#             return fresh
#         return term

#     @_unnest_term.register
#     def _(self, term: ast.TermFunction) -> ArgumentAST:
#         new_pool = [
#             ast.ArgumentTuple(
#                 self.lib, tuple(self._unnest_term(a) for a in tup.arguments)
#             )
#             for tup in term.pool
#         ]
#         new_func = term.update(self.lib, pool=new_pool)
#         if self._is_evaluable(term.name, sum(len(t.arguments) for t in new_pool)):
#             fresh = self.var_gen.fresh_variable(self.lib, term.location, "FUN")
#             comp = ast.LiteralComparison(
#                 self.lib,
#                 term.location,
#                 ast.Sign.NoSign,
#                 new_func,
#                 [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
#             )
#             self.unnested_functions.append(comp)
#             return fresh
#         return new_func

#     # @_unnest_term.register
#     # def _(self, term: ast.TermTuple) -> ArgumentAST:
#     #     new_pool = []
#     #     for elem in term.pool:
#     #         if isinstance(elem, ast.ArgumentTuple):
#     #             new_pool.append(
#     #                 ast.ArgumentTuple(
#     #                     self.lib, tuple(self._unnest_term(a) for a in elem.arguments)
#     #                 )
#     #             )
#     #         else:
#     #             new_pool.append(ast.ArgumentTuple(self.lib, (self._unnest_term(elem),)))
#     #     return term.update(self.lib, pool=new_pool)

#     @_unnest_term.register
#     def _(self, term: ast.TermAbsolute) -> ArgumentAST:
#         new_pool = [self._unnest_term(t) for t in term.pool]
#         return term.update(self.lib, pool=new_pool)

#     # @_unnest_term.register
#     # def _(self, term: ast.TermUnaryOperation) -> ArgumentAST:
#     #     return term.update(self.lib, right=self._unnest_term(term.right))

#     # @_unnest_term.register
#     # def _(self, term: ast.TermBinaryOperation) -> ArgumentAST:
#     #     return term.update(
#     #         self.lib,
#     #         left=self._unnest_term(term.left),
#     #         right=self._unnest_term(term.right),
#     #     )

#     def _unnest(self, node: Any) -> Any:
#         """
#         Generic transformer used as callback for node.transform(lib, self._unnest).

#         - If node is a TermAST (ArgumentAST), call _unnest_term to get a (possibly) new TermAST.
#         - If node is any other Clingo AST node, call its transform(lib, transformer).
#         - If node is a plain Python value (string, symbol name, etc.), return as-is.
#         """
#         # If it's a Term AST (argument), dispatch to singledispatch term handler
#         if isinstance(node, TermAST):
#             return self._unnest_term(cast(ArgumentAST, node))

#         if hasattr(node, "transform"):
#             # node.transform expects: lib, transformer
#             # and it will call transformer on children; transformer must accept Any.
#             return node.transform(self.lib, self._unnest) or node

#         # otherwise, not an AST node (string, number etc.) -> return unchanged
#         return node

#     # ----------------- Public statement entry -----------------
#     def transform_rule(self, st: StatementAST) -> StatementAST:
#         """
#         Transform a single statement (rule). Returns the transformed statement.
#         """
#         # _unnest returns Any; cast back to StatementAST as expected by callers.
#         return cast(StatementAST, self._unnest(st))
