from functools import singledispatchmethod
from typing import List, Set, Union, cast

from clingo import ast
from clingo.core import Library

from fasp.ast.syntax_checking import SymbolSignature
from fasp.ast.transformers.unnest_functions import UnnestFunctionsTransformer
from fasp.util.ast import AST, collect_variables, collect_variables_list


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
        new_head = self._rewrite(transformed_rule.head)

        # Rewrite body and, while doing so, track which comparisons are already present
        new_body: List[
            Union[ast.BodySimpleLiteral, ast.BodyConditionalLiteral, ast.BodyAggregate]
        ] = []

        # present_in_body = collect_comparisons(None)  # start empty set

        for lit in transformed_rule.body:
            rewritten = self._rewrite(lit)
            items: List = rewritten if isinstance(rewritten, list) else [rewritten]
            for it in items:
                new_body.append(it)
                # present_in_body.update(collect_comparisons(it))

        head_comps = sorted(self._comparisons_for_node(new_head), key=str)
        for comp in head_comps:
            # if comp not in present_in_body:
            new_body.append(ast.BodySimpleLiteral(self.lib, comp))
            # present_in_body.add(comp)

        # NOTE: Body can be sorted to ensure same order for tests
        # new_body = sorted(new_body, key=str)

        return ast.StatementRule(
            self.lib, transformed_rule.location, new_head, new_body
        )

    def _comparisons_for_node(
        self, node: ast.StatementRule | AST
    ) -> List[ast.LiteralComparison]:
        """Collect unnested comparisons relevant for variables in this node."""
        vars_in_node = collect_variables_list([node])
        comps: List[ast.LiteralComparison] = []
        for v in vars_in_node:
            if v in self.unnest_transformer._var_to_comp:
                comps.append(self.unnest_transformer._var_to_comp[v])
        return comps

    @singledispatchmethod
    def _rewrite(self, node: AST):
        """Fallback: recurse transform if available, else return node."""
        return node.transform(self.lib, lambda c: self._rewrite(c)) or node

    @_rewrite.register
    def _(self, node: ast.BodySimpleLiteral):
        new_lit = self._rewrite(node.literal)
        comps = self._comparisons_for_node(new_lit)

        # Negative literal (sign=Single): conditional literal
        if hasattr(new_lit, "sign") and new_lit.sign == ast.Sign.Single and comps:
            # Build #false head literal
            false_head = ast.LiteralBoolean(
                self.lib,
                new_lit.location,
                ast.Sign.NoSign,  # no sign on #false
                False,  # value = false
            )

            # Make positive version of the negated literal (drop the "not")
            positive_lit = new_lit.update(self.lib, sign=ast.Sign.NoSign)

            return ast.BodyConditionalLiteral(
                self.lib,
                new_lit.location,
                false_head,
                (positive_lit, *comps),
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
                new_condition.extend(rewritten)  # pragma: no cover
            else:
                new_condition.append(rewritten)

        new_comps: List[ast.LiteralComparison] = self._comparisons_for_node(node)

        new_condition.extend(new_comps)
        return node.update(self.lib, condition=tuple(new_condition))

    @_rewrite.register
    def _(self, node: ast.BodyAggregate):

        # Collect guard comps separately
        guard_comps: List[ast.LiteralComparison] = []
        if node.left:
            left_comps = self._comparisons_for_node(node.left)
            guard_comps.extend(left_comps)

        if node.right:
            right_comps = self._comparisons_for_node(node.right)
            guard_comps.extend(right_comps)

        # Recurse into the aggregate to handle elements
        new_node = node.transform(self.lib, lambda c: self._rewrite(c)) or node
        # Return both the rewritten aggregate and new guard comps as separate body literals
        result: List[Union[ast.BodyAggregate, ast.BodySimpleLiteral]] = [new_node]
        for comp in guard_comps:
            result.append(ast.BodySimpleLiteral(self.lib, comp))
        return result
