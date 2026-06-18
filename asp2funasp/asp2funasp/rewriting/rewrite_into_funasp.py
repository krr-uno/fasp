from collections.abc import Sequence
from functools import singledispatchmethod
from typing import List, cast

from clingo import ast
from clingo.core import Location
from funasp.fun_ast._nodes import AssignmentRule, HeadSimpleAssignment
from funasp.util.ast import (
    AST,
    ELibrary,
    function_arguments_ast,
    is_function,
)

from asp2funasp.util.types import FRelation, SymbolSignature
from asp2funasp.util.util import index_frelations

RewriteResult = AST | AssignmentRule | None


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

        if new_head is not None:
            return AssignmentRule(
                location=node.location,
                head=new_head,
                body=new_body,
            )

        if body_changed:
            return node.update(self.lib.library, body=new_body)

        return None

    def _rewrite_head(self, head: ast.HeadLiteral) -> HeadSimpleAssignment | None:
        if not isinstance(head, ast.HeadSimpleLiteral):
            return None  # missing

        literal = head.literal

        if not isinstance(literal, ast.LiteralSymbolic):
            return None

        if literal.sign != ast.Sign.NoSign:
            return None  # missing

        term = literal.atom

        if not is_function(term):
            return None  # missing

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
