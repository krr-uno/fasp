from functools import singledispatchmethod
from typing import List, Set, Tuple

from clingo import ast
from clingo.core import Library
from clingo.symbol import SymbolType

from fasp.ast.syntax_checking import SymbolSignature
from fasp.util.ast import FreshVariableGenerator, StatementAST, collect_variables


class UnnestedRuleRewriteTransformer:
    """
    Performs rule rewriting using UnnestFunctionsTransformer.
    Pipeline per rule:
      1. Unnest functions
      2. Normalize comparisons (head & body) -- using singledispatch _normalize
      3. Rewrite (base: append extra comparisons to body)
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
        return [self.rewrite_rule_with_unnested(r) for r in rules]

    def rewrite_rule_with_unnested(
        self, st: StatementAST
    ) -> Tuple[StatementAST, List[ast.LiteralComparison]]:
        """
        Pipeline for one statement:
        - Instantiate unnest transformer (it owns a FreshVariableGenerator)
        - Run unnest.transform_rule (this may produce FUN, FUN2, ...)
        - Reuse the unnester's var_gen for normalization to avoid collision
        - Normalize the statement AST (head + body) with that same generator
        - Combine unnested + normalization extras and call rewrite strategy
        """
        # non-rule statements: just run unnest & return extras (keeps behaviour)
        if not isinstance(st, ast.StatementRule):
            used_vars: set[str] = set() 
            new_st, unnested, fvg = self._unnest(st, used_vars) 
            return new_st, unnested 

        # collect used variables in the rule
        used_vars = collect_variables([st])

        # unnest the rule (returns new AST, unnested list, and shared fvg)
        new_st, unnested, fvg = self._unnest(st, used_vars)

        # normalize the statement AST (head + bodwith the same generator
        normalized_st, extra_from_norm = self._normalize(new_st, fvg)

        # combine unnested + normalization extras
        all_extra = unnested + extra_from_norm

        # rewrite strategy (for now only default: append extras to body)
        return self._rewrite(normalized_st, all_extra)

    def _unnest(
        self, st: StatementAST, used_vars: Set[str]
    ) -> Tuple[StatementAST, List[ast.LiteralComparison], FreshVariableGenerator]:
        """
        Run UnnestFunctionsTransformer on a statement.
        Returns:
            - transformed statement
            - unnested comparisons (f(...) = FUN)
            - the FreshVariableGenerator used by unnest
        """
        unnest = self.unnest_transformer_cls(
            self.lib,
            evaluable_functions=self.evaluable_functions,
            used_variable_names=used_vars,
        )
        new_st = unnest.transform_rule(st)
        return new_st, list(unnest.unnested_functions), unnest.var_gen

    @singledispatchmethod
    def _rewrite(
        self, st: StatementAST, extra_literals: List[ast.LiteralComparison]
    ) -> Tuple[StatementAST, List[ast.LiteralComparison]]:
        """
        Default rewrite: append all extra comparisons to body (as BodySimpleLiteral).
        """
        if isinstance(st, ast.StatementRule):
            existing_body = list(st.body)
            extra_body = [
                ast.BodySimpleLiteral(self.lib, comp) for comp in extra_literals
            ]
            new_body = existing_body + extra_body
            new_rule = st.update(self.lib, body=new_body)
            return new_rule, extra_literals

        return st, extra_literals # pragma: no cover # unreachable in current pipeline (non-rules are handled earlier)

    # --------------------------
    # Normalization
    # Normalization works on the statement AST (head + body) and returns
    # (possibly-updated-node, list_of_extra_comparisons_to_add).
    # --------------------------

    @singledispatchmethod
    def _normalize(self, node, fvg: FreshVariableGenerator):
        """Default: no-op."""
        return node, []

    @_normalize.register
    def _(self, st: ast.StatementRule, fvg: FreshVariableGenerator):
        extra: List[ast.LiteralComparison] = []

        # normalize head
        new_head, head_extra = self._normalize(st.head, fvg)
        extra.extend(head_extra)

        # normalize body items (keep order)
        new_body_items = []
        for lit in st.body:
            lit_norm, lit_extra = self._normalize(lit, fvg)
            new_body_items.append(lit_norm)
            extra.extend(lit_extra)

        new_st = st.update(self.lib, head=new_head, body=new_body_items)
        return new_st, extra

    @_normalize.register
    def _(self, head: ast.HeadSimpleLiteral, fvg: FreshVariableGenerator):
        # Only normalize if literal is a comparison (LiteralComparison)
        lit, extra = self._normalize(head.literal, fvg)
        return head.update(self.lib, literal=lit), extra

    @_normalize.register
    def _(self, lit: ast.BodySimpleLiteral, fvg: FreshVariableGenerator):
        inner, extra = self._normalize(lit.literal, fvg)
        return lit.update(self.lib, literal=inner), extra

    @_normalize.register
    def _(self, comp: ast.LiteralComparison, fvg: FreshVariableGenerator):
        """
        Normalize a single LiteralComparison:
        - If both sides are evaluable functions -> introduce fresh FUN and produce:
              lhs op FUN   and   rhs = FUN   (rhs-equals-FUN is returned as extra)
        - If RHS is evaluable but LHS not -> flip sides so evaluable stays RHS
        - Otherwise leave unchanged
        """

        lhs = comp.left
        rhs_guard = comp.right[0]  # only handle single right-guard chain here
        rhs = rhs_guard.term
        rel = rhs_guard.relation

        # helper to decide if a term is an evaluable function
        def term_evaluable(t) -> bool:
            if isinstance(t, ast.TermFunction):
                arity = len(t.pool[0].arguments) if t.pool else 0
                return SymbolSignature(t.name, arity) in self.evaluable_functions
            if isinstance(t, ast.TermSymbolic) and t.symbol.type == SymbolType.Function:
                arity = len(t.symbol.arguments)
                return SymbolSignature(t.symbol.name, arity) in self.evaluable_functions
            return False

        lhs_eval = term_evaluable(lhs)
        rhs_eval = term_evaluable(rhs)

        if lhs_eval and rhs_eval:
            # both evaluable: introduce fresh FUN var; return modified comp and extra rhs_eq
            fresh = fvg.fresh_variable(self.lib, comp.location, "FUN")
            rhs_eq = ast.LiteralComparison(
                self.lib,
                comp.location,
                ast.Sign.NoSign,
                rhs,
                [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
            )
            new_comp = comp.update(
                self.lib, right=[ast.RightGuard(self.lib, rel, fresh)]
            )
            return new_comp, [rhs_eq]

        if (not lhs_eval) and rhs_eval:
            # flip sides so evaluable stays on RHS
            flipped = comp.update(
                self.lib, left=rhs, right=[ast.RightGuard(self.lib, rel, lhs)]
            )
            return flipped, []

        # otherwise keep original
        return comp, []

    # default fallback for other node types (head/body aggregates etc.)
    # they are left unchanged by normalization (handled by singledispatch default)
