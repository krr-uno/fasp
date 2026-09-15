from typing import Sequence

from funasp.asp2funasp.util.types import FRelation, SymbolSignature


class FreshFunctionNameGenerator:
    def __init__(self, reserved_names: Sequence[str]):
        self.used_names = set(reserved_names)

    def fresh(self, base_name: str) -> str:
        index = 1

        while True:
            candidate = f"{base_name}_{index}"

            if candidate not in self.used_names:
                self.used_names.add(candidate)
                return candidate

            index += 1


def build_function_name_index(
    frelations: Sequence[FRelation],
    conflicting_signatures: set[SymbolSignature],
) -> dict[SymbolSignature, str]:
    """
    Decide which FUNASP function symbol name each FRelation should use.

    Rename only when the encoded functional predicate would conflict with a
    source-level symbol that survives rewriting.

    Example:
        Source contains surviving assign/2 term:
            color(assign(N,C)).

        FRelation assign/3 would be encoded as Fassign(N,C,V), which denotes
        a function whose logical input side is assign/2. That conflicts with
        surviving assign/2, so assign/3 is renamed:

            assign/3 -> assign_1
            emitted as Fassign_1(N,C,V)

    But if assign/3 itself is rewritten everywhere and no source-level assign/3
    survives, then assign/4 does not need to become assign_2.
    """
    generator = FreshFunctionNameGenerator(
        reserved_names=[frel.name for frel in frelations],
    )

    function_name_index: dict[SymbolSignature, str] = {}

    for frel in sorted(frelations, key=lambda item: (item.name, item.arity)):
        key = SymbolSignature(frel.name, frel.arity)

        # The encoded function for assign/3 semantically corresponds to
        # assign/2 := value, so it conflicts only if assign/2 survives.
        function_input_signature = SymbolSignature(frel.name, len(frel.arguments))

        if function_input_signature in conflicting_signatures:
            function_name_index[key] = generator.fresh(frel.name)
        else:
            function_name_index[key] = frel.name

    return function_name_index


from collections.abc import Sequence
from functools import singledispatchmethod

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.util.types import FRelation, SymbolSignature
from funasp.util.ast import AST, function_arguments_ast, is_function


class SurvivingSymbolSignatureCollector:
    """
    Collect source-level predicate/function signatures that survive the
    FunctionalPredicateRewriteTransformer.

    This mirrors the current transformer behavior:

    - A top-level LiteralSymbolic whose atom signature is in frelation_index
      is rewritten into the prefixed representation, so its original source
      signature does not survive.

    - A LiteralSymbolic whose atom signature is not in frelation_index survives.
      Therefore, we collect its atom signature and recursively visit its
      children. This catches nested terms such as assign(N,C) in:

          color(assign(N,C)).

    - Nested TermFunction nodes survive unless they are inside a rewritten
      LiteralSymbolic, because the transformer currently rewrites whole
      symbolic literals and does not recursively rewrite nested terms inside
      surviving atoms.
    """

    def __init__(
        self,
        lib: Library,
        frelation_index: dict[SymbolSignature, FRelation],
    ) -> None:
        self.lib = lib
        self.frelation_index = frelation_index
        self.signatures: set[SymbolSignature] = set()

    def collect(self, nodes: Sequence[AST] | AST) -> set[SymbolSignature]:
        if isinstance(nodes, Sequence):
            for node in nodes:
                self._collect(node)
        else:
            self._collect(nodes)

        return self.signatures

    @singledispatchmethod
    def _collect(self, node: AST) -> None:
        node.visit(self._collect)

    @_collect.register
    def _(self, node: ast.LiteralSymbolic) -> None:
        atom = node.atom

        if not is_function(atom):
            return

        name, arguments = function_arguments_ast(self.lib, atom)
        key = SymbolSignature(name, len(arguments))

        if key in self.frelation_index:
            # The whole literal will be rewritten to the prefixed function
            # representation, so neither this source atom nor nested terms
            # inside it survive.
            return

        self.signatures.add(key)

        # The literal survives, so nested source-level function terms inside
        # the atom also survive and can cause naming conflicts.
        node.visit(self._collect)

    @_collect.register
    def _(self, node: ast.TermFunction) -> None:
        name, arguments = function_arguments_ast(self.lib, node)
        key = SymbolSignature(name, len(arguments))
        self.signatures.add(key)

        node.visit(self._collect)

    @_collect.register
    def _(self, node: ast.TermSymbolic) -> None:
        return

    @_collect.register
    def _(self, node: ast.TermVariable) -> None:
        return
