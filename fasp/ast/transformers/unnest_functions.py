from functools import singledispatchmethod
from itertools import chain
from typing import Any, List, Set, cast

from clingo import ast
from clingo.core import Library
from clingo.symbol import SymbolType

from fasp.ast.protecting import (
    COMPARISON_NAME,
    protect_comparisons,
    restore_comparisons,
)
from fasp.ast.syntax_checking import (
    ParsingException,
    SymbolSignature,
    SyntacticError,
    get_evaluable_functions,
)
from fasp.ast.transformers.head_aggregate_rewrite import (
    HeadAggregateToBodyRewriteTransformer,
)
from fasp.util.ast import (
    AST,
    ArgumentAST,
    BodyLiteralAST,
    FreshVariableGenerator,
    LiteralAST,
    StatementAST,
    TermAST,
    collect_variables,
    create_body_literal,
    create_literal,
    function_arguments,
    function_arguments_ast,
    is_function,
)


class UnnestFunctionsTransformer:
    """
    Recursively unnest evaluable functions in Clingo 6 AST.
    Fills `unnested_functions` with LiteralComparison objects of the form:
        original_term = FUNx
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

    # ----------------- Term node handlers -----------------
    @singledispatchmethod
    def _unnest_term(self, term: ArgumentAST) -> ArgumentAST:
        # fallback: return as-is
        return term

    # @_unnest_term.register
    # def _(self, term: ast.TermVariable) -> ArgumentAST:
    #     return term

    @_unnest_term.register
    def _(self, term: ast.TermSymbolic) -> ArgumentAST:
        # Zero arity evaluable funcitons
        if term.symbol.type == SymbolType.Function and self._is_evaluable(
            term.symbol.name, 0
        ):
            fresh = self.var_gen.fresh_variable(self.lib, term.location, "FUN")
            comp = ast.LiteralComparison(
                self.lib,
                term.location,
                ast.Sign.NoSign,
                term,
                [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
            )
            self.unnested_functions.append(comp)
            return fresh
        return term

    @_unnest_term.register
    def _(self, term: ast.TermFunction) -> ArgumentAST:
        new_pool = [
            ast.ArgumentTuple(
                self.lib, tuple(self._unnest_term(a) for a in tup.arguments)
            )
            for tup in term.pool
        ]
        new_func = term.update(self.lib, pool=new_pool)
        if self._is_evaluable(term.name, sum(len(t.arguments) for t in new_pool)):
            fresh = self.var_gen.fresh_variable(self.lib, term.location, "FUN")
            comp = ast.LiteralComparison(
                self.lib,
                term.location,
                ast.Sign.NoSign,
                new_func,
                [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
            )
            self.unnested_functions.append(comp)
            return fresh
        return new_func

    # @_unnest_term.register
    # def _(self, term: ast.TermTuple) -> ArgumentAST:
    #     new_pool = []
    #     for elem in term.pool:
    #         if isinstance(elem, ast.ArgumentTuple):
    #             new_pool.append(
    #                 ast.ArgumentTuple(
    #                     self.lib, tuple(self._unnest_term(a) for a in elem.arguments)
    #                 )
    #             )
    #         else:
    #             new_pool.append(ast.ArgumentTuple(self.lib, (self._unnest_term(elem),)))
    #     return term.update(self.lib, pool=new_pool)

    @_unnest_term.register
    def _(self, term: ast.TermAbsolute) -> ArgumentAST:
        new_pool = [self._unnest_term(t) for t in term.pool]
        return term.update(self.lib, pool=new_pool)

    # @_unnest_term.register
    # def _(self, term: ast.TermUnaryOperation) -> ArgumentAST:
    #     return term.update(self.lib, right=self._unnest_term(term.right))

    # @_unnest_term.register
    # def _(self, term: ast.TermBinaryOperation) -> ArgumentAST:
    #     return term.update(
    #         self.lib,
    #         left=self._unnest_term(term.left),
    #         right=self._unnest_term(term.right),
    #     )

    def _unnest(self, node: Any) -> Any:
        """
        Generic transformer used as callback for node.transform(lib, self._unnest).

        - If node is a TermAST (ArgumentAST), call _unnest_term to get a (possibly) new TermAST.
        - If node is any other Clingo AST node, call its transform(lib, transformer).
        - If node is a plain Python value (string, symbol name, etc.), return as-is.
        """
        # If it's a Term AST (argument), dispatch to singledispatch term handler
        if isinstance(node, TermAST):
            return self._unnest_term(cast(ArgumentAST, node))

        if hasattr(node, "transform"):
            # node.transform expects: lib, transformer
            # and it will call transformer on children; transformer must accept Any.
            return node.transform(self.lib, self._unnest) or node

        # otherwise, not an AST node (string, number etc.) -> return unchanged
        return node

    # ----------------- Public statement entry -----------------
    def transform_rule(self, st: StatementAST) -> StatementAST:
        """
        Transform a single statement (rule). Returns the transformed statement.
        """
        # _unnest returns Any; cast back to StatementAST as expected by callers.
        return cast(StatementAST, self._unnest(st))
