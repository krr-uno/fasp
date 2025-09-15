from functools import singledispatchmethod
from typing import List, Set, Tuple

from clingo import ast
from clingo.core import Library

from fasp.ast.syntax_checking import SymbolSignature
from fasp.util.ast import StatementAST, collect_variables


class RewriteWithUnnestedTransformer:
    """
    Performs rule rewriting using UnnestFunctionsTransformer.
    Different rule types can be handled via singledispatch.
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
        unnest_transformer_cls,
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.unnest_transformer_cls = unnest_transformer_cls

    def rewrite_rules_with_unnested(
        self, rules: List[StatementAST]
    ) -> List[Tuple[StatementAST, List[ast.LiteralComparison]]]:
        out: List[Tuple[StatementAST, List[ast.LiteralComparison]]] = []
        for r in rules:
            out.append(self.rewrite_rule_with_unnested(r))
        return out

    def rewrite_rule_with_unnested(
        self, st: StatementAST
    ) -> Tuple[StatementAST, List[ast.LiteralComparison]]:
        return self._rewrite(st)

    @singledispatchmethod
    def _rewrite(
        self, st: StatementAST
    ) -> Tuple[StatementAST, List[ast.LiteralComparison]]:
        """
        Default rewrite: append all unnested comparisons to body.
        """
        # per-rule used vars (only rules)
        used_vars = (
            collect_variables([st]) if isinstance(st, ast.StatementRule) else set()
        )

        # fresh transformer per rule
        unnest = self.unnest_transformer_cls(
            self.lib,
            evaluable_functions=self.evaluable_functions,
            used_variable_names=used_vars,
        )
        new_st = unnest.transform_rule(st)

        if isinstance(new_st, ast.StatementRule):
            existing_body = list(new_st.body)
            extra_body = [
                ast.BodySimpleLiteral(self.lib, comp)
                for comp in unnest.unnested_functions
            ]
            new_body = existing_body + extra_body
            new_rule = new_st.update(self.lib, body=new_body)
            return new_rule, list(unnest.unnested_functions)

        return new_st, list(unnest.unnested_functions)
