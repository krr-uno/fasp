from functools import singledispatchmethod
from typing import List, Set, Union, cast

from clingo import ast
from clingo.core import Library

from fasp.ast.syntax_checking import SymbolSignature
from fasp.ast.transformers.unnest_functions import UnnestFunctionsTransformer
from fasp.util.ast import AST, collect_comparisons, collect_variables


class RuleRewriteTransformer:
    """
    Transformer that rewrites entire rules by applying unnesting strategies
    contextually based on where functions appear in the rule structure.
    """

    def __init__(self, lib: Library, evaluable_functions: Set[SymbolSignature]):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.unnest_transformer: UnnestFunctionsTransformer

    def transform_rule(self, rule: ast.StatementRule) -> ast.StatementRule:
        """Transform a complete rule statement."""
        used_vars = collect_variables([rule])
        self.unnest_transformer = UnnestFunctionsTransformer(
            self.lib, self.evaluable_functions, used_vars
        )

        # Unnest functions
        transformed_rule = self.unnest_transformer.transform_rule(rule)
        transformed_rule = cast(ast.StatementRule, transformed_rule)

        # Rewrite head
        new_head_node = self._rewrite(transformed_rule.head)
        new_head = new_head_node

        # Rewrite body and, while doing so, track which comparisons are already present
        new_body: List[
            Union[ast.BodySimpleLiteral, ast.BodyConditionalLiteral, ast.BodyAggregate]
        ] = []

        present_in_body = collect_comparisons([])  # start empty set

        for lit in transformed_rule.body:
            rewritten = self._rewrite(lit)
            items: List = rewritten if isinstance(rewritten, list) else [rewritten]
            for it in items:
                new_body.append(it)
                present_in_body.update(collect_comparisons(it))

        head_comps = self._comparisons_for_node(new_head_node)
        for comp in head_comps:
            if comp not in present_in_body:
                new_body.append(ast.BodySimpleLiteral(self.lib, comp))
                present_in_body.add(comp)

        return ast.StatementRule(
            self.lib, transformed_rule.location, new_head, new_body
        )

    def _comparisons_for_node(
        self, node: ast.StatementRule
    ) -> List[ast.LiteralComparison]:
        """Collect unnested comparisons relevant for variables in this node."""
        vars_in_node = collect_variables([node])
        comps = []
        for v in vars_in_node:
            if v in self.unnest_transformer._var_to_comp:
                comps.append(self.unnest_transformer._var_to_comp[v])
        return comps

    @singledispatchmethod
    def _rewrite(self, node: AST):
        """Fallback: recurse transform if available, else return node."""
        return node.transform(self.lib, lambda c: self._rewrite(c)) or node

    # @_rewrite.register
    # def _(self, node: ast.HeadSimpleLiteral):
    #     new_lit = self._rewrite(node.literal)
    #     return node.update(self.lib, literal=new_lit)

    @_rewrite.register
    def _(self, node: ast.BodySimpleLiteral):
        new_lit = self._rewrite(node.literal)
        comps = self._comparisons_for_node(new_lit)

        # Negative literal (sign=Single): conditional literal
        if hasattr(new_lit, "sign") and new_lit.sign == ast.Sign.Single and comps:
            return ast.BodyConditionalLiteral(
                self.lib, new_lit.location, new_lit, tuple(comps)
            )
        else:
            result: List[ast.BodySimpleLiteral] = [
                node.update(self.lib, literal=new_lit)
            ]
            for c in comps:
                result.append(ast.BodySimpleLiteral(self.lib, c))
            return result

    @_rewrite.register
    def _(self, node: ast.BodyAggregateElement):
        # Rewrite each condition atom
        new_condition = []
        for c in node.condition:
            rewritten = self._rewrite(c)
            if isinstance(rewritten, list):
                new_condition.extend(rewritten) #pragma: no cover
            else:
                new_condition.append(rewritten)

        # Collect comparisons from variables in this element
        comps: List[ast.LiteralComparison] = []
        for cond in new_condition:
            comps.extend(self._comparisons_for_node(cond))

        for comp in self.unnest_transformer.unnested_functions:
            if comp not in comps:
                comps.append(comp)

        new_condition.extend(comps)
        return node.update(self.lib, condition=tuple(new_condition))

    # @_rewrite.register
    # def _(self, node: ast.BodyAggregate):
    #     new_elements = [self._rewrite(e) for e in node.elements]
    #     return node.update(self.lib, elements=tuple(new_elements))
