from functools import singledispatchmethod
from typing import List, Set

from clingo import ast
from clingo.core import Library

from fasp.ast._nodes import FASP_AST, AssignmentRule
from fasp.ast.collectors import SymbolSignature, collect_variables
from fasp.ast.rewritings.unnesting import unnest_functions
from fasp.util.ast import (
    AST,
    FreshVariableGenerator,
)


class RuleRewriteTransformer:
    """
    Performs rule-level rewriting by applying `unnest_functions`
    across all relevant AST substructures.
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
        *,
        evaluable_functions_allowed_in_negated_literals: bool = True,
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.evaluable_functions_allowed_in_negated_literals = (
            evaluable_functions_allowed_in_negated_literals
        )

    def transform_rule(self, node: FASP_AST) -> FASP_AST:
        """
        Entrypoint for rewriting an entire statement.
        """
        used = collect_variables(node)
        var_gen = FreshVariableGenerator(used)
        return self._transform(node, var_gen)

    @singledispatchmethod
    def _transform(self, node: FASP_AST, var_gen: FreshVariableGenerator) -> FASP_AST:
        """Default: return node unchanged."""
        return node

    # Rule Statements
    @_transform.register
    def _(
        self, node: ast.StatementRule, var_gen: FreshVariableGenerator
    ) -> ast.StatementRule:
        new_head, head_comps = unnest_functions(
            self.lib, node.head, self.evaluable_functions, var_gen
        )

        new_body_literals = []

        for lit in node.body:
            new_lit = self._transform(lit, var_gen)
            new_lit, comps = unnest_functions(
                self.lib, new_lit, self.evaluable_functions, var_gen
            )

            new_body_literals.append(new_lit)
            for comp in comps:
                new_body_literals.append(ast.BodySimpleLiteral(self.lib, literal=comp))

        # Head comparisons also belong in body
        for comp in head_comps:
            new_body_literals.append(ast.BodySimpleLiteral(self.lib, literal=comp))

        return node.update(self.lib, head=new_head, body=new_body_literals)

    @_transform.register
    def _(
        self, node: AssignmentRule, var_gen: FreshVariableGenerator
    ) -> AssignmentRule:
        new_head, head_comps = unnest_functions(
            self.lib, node.head, self.evaluable_functions, var_gen
        )
        new_body = []
        for lit in node.body:
            new_lit, comps = unnest_functions(
                self.lib, lit, self.evaluable_functions, var_gen
            )
            new_body.append(new_lit)
            new_body.extend(comps)
        new_body.extend(head_comps)
        return node.update(head=new_head, body=new_body)

    # Aggregates
    @_transform.register
    def _(
        self, node: ast.BodyAggregate, var_gen: FreshVariableGenerator
    ) -> ast.BodyAggregate:
        new_elements = []
        for elem in node.elements:
            new_elem = self._transform(elem, var_gen)
            new_elements.append(new_elem)
        return node.update(self.lib, elements=new_elements)

    @_transform.register
    def _(
        self, node: ast.BodyAggregateElement, var_gen: FreshVariableGenerator
    ) -> ast.BodyAggregateElement:

        # Unnest tuple terms
        new_tuple = []
        local_comps: List[ast.LiteralComparison] = []

        for t in node.tuple:
            new_t, comps = unnest_functions(
                self.lib, t, self.evaluable_functions, var_gen, outer=False
            )
            new_tuple.append(new_t)
            local_comps.extend(comps)

        # Unnest conditions (e.g. p(g(Y)), q(X))
        new_condition = []
        for cond in node.condition:
            new_c, comps = unnest_functions(
                self.lib, cond, self.evaluable_functions, var_gen
            )
            new_condition.append(new_c)
            local_comps.extend(comps)

        new_condition.extend(local_comps)

        return node.update(self.lib, tuple=new_tuple, condition=new_condition)
