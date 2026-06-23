from collections.abc import Sequence
from functools import singledispatchmethod
from typing import List, cast

from clingo import ast
from clingo.core import Location
from funasp.fun_ast._nodes import (
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    HeadAggregateAssignment,
    HeadAggregateAssignmentElement,
    HeadSimpleAssignment,
)
from funasp.util.ast import (
    AST,
    ELibrary,
    function_arguments_ast,
    is_function,
)

from asp2funasp.util.types import FRelation, SymbolSignature
from asp2funasp.util.util import index_frelations

RewriteResult = AST | AssignmentRule | None

HeadRewriteResult = (
    ast.HeadLiteral
    | HeadSimpleAssignment
    | ChoiceAssignment
    | HeadAggregateAssignment
    | None
)

AssignmentHead = HeadSimpleAssignment | ChoiceAssignment | HeadAggregateAssignment


class FunctionalPredicateRewriteTransformer:
    def __init__(self, lib: ELibrary, frelations: List[FRelation]):
        self.lib = lib
        self.frelation_index = index_frelations(frelations)

    def transform_rule(self, node: AST) -> RewriteResult:
        return self._rewrite(node)

    def _split_functional_atom(
        self,
        location: Location,
        name: str,
        arguments: Sequence[ast.TermOrProjection],
        frel: FRelation,
    ) -> tuple[ast.TermFunction, ast.Term]:
        """
        Build the FUNASP-style function term and value term for a functional
        predicate atom.

        Example:
            p(X, Y, Z)

        With:
            frel.arguments = [0]
            frel.values = [[1, 2]]

        Becomes:
            lhs = p(X)
            rhs = (Y, Z)
        """
        lhs_args = [arguments[i] for i in frel.arguments]

        # function_arguments_ast() can return TermOrProjection, but for the
        # functional value side used in comparisons/assignments we need terms.
        rhs_terms = [
            cast(ast.Term, arguments[i])
            for value_group in frel.values
            for i in value_group
        ]

        lhs_args_tuple = ast.ArgumentTuple(self.lib.library, lhs_args)
        lhs = ast.TermFunction(
            self.lib.library,
            location,
            name,
            [lhs_args_tuple],
        )

        rhs: ast.Term = (
            rhs_terms[0]
            if len(rhs_terms) == 1
            else ast.TermTuple(self.lib.library, location, rhs_terms)
        )

        return lhs, rhs

    def _rewrite_symbolic_literal_as_assignment(
        self,
        literal: ast.LiteralSymbolic,
    ) -> HeadSimpleAssignment | None:
        if literal.sign != ast.Sign.NoSign:
            return None

        term = literal.atom

        if not is_function(term):
            return None

        name, arguments = function_arguments_ast(self.lib.library, term)
        key = SymbolSignature(name, len(arguments))

        if key not in self.frelation_index:
            return None

        frel = self.frelation_index[key]

        assigned_function, value = self._split_functional_atom(
            literal.location,
            name,
            arguments,
            frel,
        )

        return HeadSimpleAssignment(
            location=literal.location,
            assigned_function=assigned_function,
            value=value,
        )

    def _rewrite_condition(
        self,
        condition: Sequence[ast.Literal],
    ) -> tuple[List[ast.Literal], bool]:
        new_condition: List[ast.Literal] = []
        changed = False

        for lit in condition:
            new_lit = self._rewrite(lit)
            if new_lit is not None:
                changed = True
                assert isinstance(new_lit, ast.Literal)
                new_condition.append(new_lit)
            else:
                new_condition.append(lit)

        return new_condition, changed

    def _rewrite_head_conditional_literal(
        self,
        node: ast.HeadConditionalLiteral,
    ) -> tuple[ast.HeadConditionalLiteral, bool]:
        new_condition, condition_changed = self._rewrite_condition(node.condition)

        if not condition_changed:
            return node, False

        return (
            node.update(
                self.lib.library,
                condition=new_condition,
            ),
            True,
        )

    def _rewrite_set_aggregate_element(
        self,
        element: ast.SetAggregateElement,
    ) -> tuple[AssignmentAggregateElement | ast.SetAggregateElement, bool, bool]:
        """
        Rewrite one element of a HeadSetAggregate.

        Example:
            { assign(N,C) : color(C) }

        becomes:
            { assign(N) := C : color(C) }

        Returns:
            new_element:
                Either a FUNASP AssignmentAggregateElement or the original/updated
                clingo SetAggregateElement.
            changed:
                True if either the element literal or its condition changed.
            assignment_changed:
                True if the element literal became an assignment.
        """
        new_condition, condition_changed = self._rewrite_condition(element.condition)

        literal = element.literal

        if not isinstance(literal, ast.LiteralSymbolic):
            if condition_changed:
                return (
                    element.update(
                        self.lib.library,
                        condition=new_condition,
                    ),
                    True,
                    False,
                )
            return element, False, False

        assignment = self._rewrite_symbolic_literal_as_assignment(literal)

        if assignment is None:
            if condition_changed:
                return (
                    element.update(
                        self.lib.library,
                        condition=new_condition,
                    ),
                    True,
                    False,
                )
            return element, False, False

        return (
            AssignmentAggregateElement(
                location=literal.location,
                assignment=assignment,
                condition=new_condition,
            ),
            True,
            True,
        )

    def _rewrite_head_aggregate_element(
        self,
        element: ast.HeadAggregateElement,
    ) -> tuple[ast.HeadAggregateElement, bool]:
        """
        Conservatively rewrite only conditions inside a clingo HeadAggregateElement.

        Do NOT convert tuple terms like assign(N,C) into HeadAggregateAssignmentElement
        here. A head aggregate such as:

            #count { assign(N,C) : color(C) } = 1

        is not the same as a valid FUNASP assignment aggregate, and rewriting the
        tuple term to:

            #count { assign(N) := C }

        drops/changes semantics.
        """
        new_condition, condition_changed = self._rewrite_condition(element.condition)

        if not condition_changed:
            return element, False

        return (
            element.update(
                self.lib.library,
                condition=new_condition,
            ),
            True,
        )

    @singledispatchmethod
    def _rewrite(self, node: AST) -> RewriteResult:
        return node.transform(self.lib.library, self._rewrite)

    @_rewrite.register
    def _(self, node: ast.StatementRule) -> ast.StatementRule | AssignmentRule | None:
        new_body: List[ast.BodyLiteral] = []
        body_changed = False

        for lit in node.body:
            new_lit = self._rewrite(lit)
            if new_lit is not None:
                body_changed = True
                assert isinstance(new_lit, ast.BodyLiteral)
                new_body.append(new_lit)
            else:
                new_body.append(lit)

        new_head = self._rewrite_head(node.head)

        if isinstance(new_head, AssignmentHead):
            return AssignmentRule(
                location=node.location,
                head=new_head,
                body=new_body,
            )

        if new_head is not None:
            assert isinstance(new_head, ast.HeadLiteral)
            return node.update(
                self.lib.library,
                head=new_head,
                body=new_body if body_changed else node.body,
            )

        if body_changed:
            return node.update(self.lib.library, body=new_body)

        return None

    @_rewrite.register
    def _(self, node: ast.LiteralSymbolic) -> ast.LiteralComparison | None:
        term = node.atom

        if not is_function(term):
            return None  # missing

        name, arguments = function_arguments_ast(self.lib.library, term)

        key = SymbolSignature(name, len(arguments))
        if key not in self.frelation_index:
            return None

        frel = self.frelation_index[key]

        lhs, rhs = self._split_functional_atom(
            node.location,
            name,
            arguments,
            frel,
        )

        guard = ast.RightGuard(
            self.lib.library,
            ast.Relation.Equal,
            rhs,
        )

        return ast.LiteralComparison(
            self.lib.library,
            node.location,
            node.sign,
            lhs,
            [guard],
        )

    @singledispatchmethod
    def _rewrite_head(self, node: ast.HeadLiteral) -> HeadRewriteResult:
        return None

    @_rewrite_head.register
    def _(self, node: ast.HeadSimpleLiteral) -> HeadSimpleAssignment | None:
        literal = node.literal

        if not isinstance(literal, ast.LiteralSymbolic):
            return None

        return self._rewrite_symbolic_literal_as_assignment(literal)

    @_rewrite_head.register
    def _(
        self, node: ast.HeadSetAggregate
    ) -> ChoiceAssignment | ast.HeadSetAggregate | None:
        new_elements: List[AssignmentAggregateElement | ast.SetAggregateElement] = []
        changed = False
        assignment_changed = False

        for element in node.elements:
            new_element, element_changed, element_assignment_changed = (
                self._rewrite_set_aggregate_element(element)
            )

            changed = changed or element_changed
            assignment_changed = assignment_changed or element_assignment_changed
            new_elements.append(new_element)

        if not changed:
            return None

        if assignment_changed:
            return ChoiceAssignment(
                location=node.location,
                left=node.left,
                elements=new_elements,
                right=node.right,
            )

        return node.update(
            self.lib.library,
            elements=cast(List[ast.SetAggregateElement], new_elements),
        )

    @_rewrite_head.register
    def _(self, node: ast.HeadAggregate) -> ast.HeadAggregate | None:
        new_elements: List[ast.HeadAggregateElement] = []
        changed = False

        for element in node.elements:
            new_element, element_changed = self._rewrite_head_aggregate_element(element)
            changed = changed or element_changed
            new_elements.append(new_element)

        if not changed:
            return None

        return node.update(
            self.lib.library,
            elements=new_elements,
        )
