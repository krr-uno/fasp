from collections.abc import Sequence
from functools import singledispatchmethod

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.util.types import FRelation, SymbolSignature
from funasp.asp2funasp.util.util import index_frelations
from funasp.util.ast import AST, function_arguments_ast, is_function


def remove_frelations_in_head_disjunctions(
    lib: Library,
    nodes: Sequence[AST],
    frelations: Sequence[FRelation],
) -> list[FRelation]:
    """
    Remove FRelations whose predicate signatures occur in head disjunctions.

    This is needed because the FUNASP solver currently does not support
    functional predicate rewrites inside disjunctive heads.
    """
    frelation_index = index_frelations(list(frelations))

    blocked_signatures = HeadDisjunctionFRelationCollector(
        lib,
        frelation_index,
    ).collect(nodes)

    return [
        frel
        for frel in frelations
        if SymbolSignature(frel.name, frel.arity) not in blocked_signatures
    ]


class HeadDisjunctionFRelationCollector:
    """
    Collect FRelation signatures that occur in head disjunctions.

    This collector only cares about disjunctive heads:

        p(X,Y) | q(X) :- body.

    If p/2 is in frelations, then p/2 is marked blocked, so it will not be
    rewritten later.

    For HeadConditionalLiteral elements:

        p(X,Y) : cond(X)

    the disjunct literal p(X,Y) is checked. By default, the condition is not
    treated as a disjunctive head literal, because it behaves more like a local
    condition/body.
    """

    def __init__(
        self,
        lib: Library,
        frelation_index: dict[SymbolSignature, FRelation],
    ) -> None:
        self.lib = lib
        self.frelation_index = frelation_index
        self.blocked_signatures: set[SymbolSignature] = set()

    def collect(self, nodes: Sequence[AST]) -> set[SymbolSignature]:
        for node in nodes:
            self._collect(node)

        return self.blocked_signatures

    @singledispatchmethod
    def _collect(self, node: AST) -> None:
        node.visit(self._collect)

    @_collect.register
    def _(self, node: ast.StatementRule) -> None:
        # Only inspect the head specially. Body occurrences are safe for the
        # normal rewrite pass and should not remove FRelations.
        self._collect_head(node.head)

    @singledispatchmethod
    def _collect_head(self, node: ast.HeadLiteral) -> None:
        return None

    @_collect_head.register
    def _(self, node: ast.HeadDisjunction) -> None:
        for element in node.elements:
            self._collect_disjunction_element(element)

    @singledispatchmethod
    def _collect_disjunction_element(
        self,
        node: ast.Literal | ast.HeadConditionalLiteral,
    ) -> None:
        return None

    @_collect_disjunction_element.register
    def _(self, node: ast.LiteralSymbolic) -> None:
        self._block_literal_if_frelation(node)

    def _block_literal_if_frelation(self, literal: ast.LiteralSymbolic) -> None:
        atom = literal.atom

        if not is_function(atom):
            return

        name, arguments = function_arguments_ast(self.lib, atom)
        key = SymbolSignature(name, len(arguments))

        if key in self.frelation_index:
            self.blocked_signatures.add(key)
