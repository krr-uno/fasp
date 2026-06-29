from collections.abc import Sequence
from functools import singledispatchmethod
from typing import List, cast

from clingo_funasp import ast
from clingo_funasp.core import Library, Location
from funasp.ast._core import PARSER_PREFIX
from funasp.util.ast import AST, function_arguments_ast, is_function

from asp2funasp.rewriting.util import (
    SurvivingSymbolSignatureCollector,
    build_function_name_index,
)
from asp2funasp.util.types import FRelation, SymbolSignature
from asp2funasp.util.util import index_frelations

RewriteResult = AST | None
HeadRewriteResult = ast.HeadLiteral | None


class FunctionalPredicateRewriteTransformer:
    def __init__(
        self,
        lib: Library,
        frelations: List[FRelation],
        conflicting_signatures: set[SymbolSignature] | None = None,
    ):
        self.lib = lib
        self.frelation_index = index_frelations(frelations)
        self.function_name_index = build_function_name_index(
            frelations,
            conflicting_signatures or set(),
        )

    @classmethod
    def from_program(
        cls,
        lib: Library,
        frelations: List[FRelation],
        nodes: Sequence[AST],
    ) -> "FunctionalPredicateRewriteTransformer":
        frelation_index = index_frelations(frelations)
        conflicting_signatures = SurvivingSymbolSignatureCollector(
            lib,
            frelation_index,
        ).collect(nodes)

        return cls(
            lib,
            frelations,
            conflicting_signatures=conflicting_signatures,
        )

    def transform_rule(self, node: AST) -> RewriteResult:
        return self._rewrite(node)

    def _function_name(self, key: SymbolSignature) -> str:
        name = self.function_name_index[key]
        return f"{PARSER_PREFIX}{name}"

    def _split_functional_atom(
        self,
        key: SymbolSignature,
        arguments: Sequence[ast.TermOrProjection],
        frel: FRelation,
    ) -> tuple[str, ast.ArgumentTuple]:
        """
        Convert p(args..., values...) into the new parser representation:

            p(X,Y) with FRelation arguments=(0,), values=[(1,)]
            -> Fp(X,Y)

        TermFunction expects an iterable of ArgumentTuple, so this returns one
        ArgumentTuple containing the rewritten arguments.
        """
        lhs_args = [arguments[i] for i in frel.arguments]

        rhs_terms = [
            cast(ast.TermOrProjection, arguments[i])
            for value_group in frel.values
            for i in value_group
        ]

        prefixed_arguments = [*lhs_args, *rhs_terms]

        return (
            self._function_name(key),
            ast.ArgumentTuple(self.lib, prefixed_arguments),
        )

    def _rewrite_function_atom(
        self,
        location: Location,
        term: ast.Term,
    ) -> ast.TermFunction | None:
        if not is_function(term):
            return None

        name, arguments = function_arguments_ast(self.lib, term)
        key = SymbolSignature(name, len(arguments))

        if key not in self.frelation_index:
            return None

        frel = self.frelation_index[key]
        prefixed_name, prefixed_argument_tuple = self._split_functional_atom(
            key,
            arguments,
            frel,
        )

        return ast.TermFunction(
            self.lib,
            location,
            prefixed_name,
            [prefixed_argument_tuple],
        )

    def _rewrite_symbolic_literal_as_prefixed_literal(
        self,
        literal: ast.LiteralSymbolic,
    ) -> ast.LiteralSymbolic | None:
        new_atom = self._rewrite_function_atom(
            literal.location,
            literal.atom,
        )

        if new_atom is None:
            return None

        return ast.LiteralSymbolic(
            self.lib,
            literal.location,
            literal.sign,
            new_atom,
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

        new_literal = self._rewrite(node.literal)
        literal_changed = new_literal is not None

        if literal_changed:
            assert isinstance(new_literal, ast.Literal)
        else:
            new_literal = node.literal

        if not condition_changed and not literal_changed:
            return node, False

        return (
            node.update(
                self.lib,
                literal=new_literal,
                condition=new_condition,
            ),
            True,
        )

    def _rewrite_set_aggregate_element(
        self,
        element: ast.SetAggregateElement,
    ) -> tuple[ast.SetAggregateElement, bool]:
        new_condition, condition_changed = self._rewrite_condition(element.condition)

        new_literal = self._rewrite(element.literal)
        literal_changed = new_literal is not None

        if literal_changed:
            assert isinstance(new_literal, ast.Literal)
        else:
            new_literal = element.literal

        if not condition_changed and not literal_changed:
            return element, False

        return (
            element.update(
                self.lib,
                literal=new_literal,
                condition=new_condition,
            ),
            True,
        )

    def _rewrite_head_aggregate_element(
        self,
        element: ast.HeadAggregateElement,
    ) -> tuple[ast.HeadAggregateElement, bool]:
        new_condition, condition_changed = self._rewrite_condition(element.condition)

        # Conservative migration:
        # only rewrite conditions for head aggregates.
        #
        # Do not rewrite arbitrary aggregate tuple terms into assignment syntax.
        # In the new parser representation, assignment aggregate syntax has
        # specific clingo shapes produced by clingo_funasp; asp2funasp should
        # not guess those shapes unless the source pattern is explicitly known.
        if not condition_changed:
            return element, False

        return (
            element.update(
                self.lib,
                condition=new_condition,
            ),
            True,
        )

    def _rewrite_head_disjunction_element(
        self,
        element: ast.Literal | ast.HeadConditionalLiteral,
    ) -> tuple[ast.Literal | ast.HeadConditionalLiteral, bool]:
        if isinstance(element, ast.HeadConditionalLiteral):
            return self._rewrite_head_conditional_literal(element)

        return element, False
        # new_element = self._rewrite(element)

        # if new_element is None:
        #     return element, False

        # assert isinstance(new_element, ast.Literal)
        # return new_element, True

    @singledispatchmethod
    def _rewrite(self, node: AST) -> RewriteResult:
        return node.transform(self.lib, self._rewrite)

    @_rewrite.register
    def _(self, node: ast.StatementRule) -> ast.StatementRule | None:
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
        head_changed = new_head is not None

        if not head_changed and not body_changed:
            return None

        return node.update(
            self.lib,
            head=new_head if head_changed else node.head,
            body=new_body if body_changed else node.body,
        )

    @_rewrite.register
    def _(self, node: ast.LiteralSymbolic) -> ast.LiteralSymbolic | None:
        return self._rewrite_symbolic_literal_as_prefixed_literal(node)

    @singledispatchmethod
    def _rewrite_head(self, node: ast.HeadLiteral) -> HeadRewriteResult:
        return None  # pragma: no cover

    @_rewrite_head.register
    def _(self, node: ast.HeadSimpleLiteral) -> ast.HeadSimpleLiteral | None:
        new_literal = self._rewrite(node.literal)

        if new_literal is None:
            return None

        assert isinstance(new_literal, ast.Literal)

        return node.update(
            self.lib,
            literal=new_literal,
        )

    @_rewrite_head.register
    def _(self, node: ast.HeadSetAggregate) -> ast.HeadSetAggregate | None:
        new_elements: List[ast.SetAggregateElement] = []
        changed = False

        for element in node.elements:
            new_element, element_changed = self._rewrite_set_aggregate_element(element)
            changed = changed or element_changed
            new_elements.append(new_element)

        if not changed:
            return None

        return node.update(
            self.lib,
            elements=new_elements,
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
            self.lib,
            elements=new_elements,
        )

    @_rewrite_head.register
    def _(self, node: ast.HeadDisjunction) -> ast.HeadDisjunction | None:
        new_elements: List[ast.Literal | ast.HeadConditionalLiteral] = []
        changed = False

        for element in node.elements:
            new_element, element_changed = self._rewrite_head_disjunction_element(
                element
            )
            changed = changed or element_changed
            new_elements.append(new_element)

        if not changed:
            return None

        return node.update(
            self.lib,
            elements=new_elements,
        )
