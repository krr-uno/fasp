from functools import singledispatchmethod
from typing import List

from clingo import ast
from clingo.core import Library
from fasp.util.ast import (
    AST,
    function_arguments,
    function_arguments_ast,
    is_function,
)

from asp2funasp.util.types import FRelation, SymbolSignature
from asp2funasp.util.util import index_frelations


class FunctionalBodyRewriteTransformer:
    def __init__(self, lib: Library, frelations: List[FRelation]):
        self.lib = lib
        self.frelation_index = index_frelations(frelations)

    def transform_rule(self, node: AST) -> None | AST:
        return self._rewrite(node)

    @singledispatchmethod
    def _rewrite(self, node: AST) -> None | AST:
        return node.transform(self.lib, self._rewrite)

    @_rewrite.register
    def _(self, node: ast.StatementRule) -> ast.StatementRule | None:
        new_body = []
        changed = False

        for lit in node.body:
            new_lit = self._rewrite(lit)
            if new_lit is not None:
                changed = True
                new_body.append(new_lit)
            else:
                new_body.append(lit)

        if not changed or new_body is None:
            return None

        return node.update(self.lib, body=new_body)

    @_rewrite.register
    def _(self, node: ast.LiteralSymbolic) -> ast.LiteralComparison | None:
        term = node.atom

        # Only handle function atoms
        assert is_function(term)

        name, arguments = function_arguments_ast(self.lib, term)

        key = SymbolSignature(name, len(arguments))
        if key not in self.frelation_index:
            return None

        frel = self.frelation_index[key]

        # BUILD LHS: function(args)
        lhs_args = [arguments[i] for i in frel.arguments]

        lhs_args_tuple = ast.ArgumentTuple(self.lib, lhs_args)
        lhs = ast.TermFunction(
            self.lib,
            node.location,
            name,
            [lhs_args_tuple],
        )

        # BUILD RHS: value(s)
        rhs_terms = [arguments[i] for v in frel.values for i in v]

        rhs = (
            rhs_terms[0]
            if len(rhs_terms) == 1
            else ast.TermTuple(self.lib, node.location, rhs_terms)
        )

        # BUILD COMPARISON
        guard = ast.RightGuard(
            self.lib,
            ast.Relation.Equal,
            rhs,
        )

        comp = ast.LiteralComparison(
            self.lib,
            node.location,
            node.sign,
            lhs,
            [guard],
        )

        return comp
