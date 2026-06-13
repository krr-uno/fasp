"""
Unit tests for ``funasp.rewriting.unnesting`` paths that the integrated
pipeline cannot reach (negated body literals are converted to conditional
literals before unnesting runs there).
"""

import unittest

from clingo_funasp import ast, symbol
from clingo_funasp.core import Location, Position

from funasp.rewriting._context import RewriteContext
from funasp.rewriting.literals import UnnestFunctionsInLiteralsTransformer
from funasp.rewriting.types import SymbolSignature
from funasp.rewriting.unnesting import unnest_statement
from funasp.ast import parse_string
from funasp.core import Library
from funasp.util.ast import FreshVariableGenerator


class TestUnnestStatement(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def unnest(self, code: str, evaluable: set[str]) -> list[str]:
        """Parse a program and unnest each statement directly."""
        context = RewriteContext(
            self.lib,
            "F",
            evaluable_functions={
                SymbolSignature(name, int(arity))
                for name, arity in (s.split("/") for s in evaluable)
            },
        )
        statements = parse_string(self.lib, code)
        return [str(unnest_statement(context, s.original)) for s in statements[1:]]

    def test_negated_body_literal(self):
        """A negated body literal with an evaluable function becomes conditional."""
        self.assertEqual(
            self.unnest("p(X) :- q(X); not r(f(X)).", {"f/1"}),
            ["p(X) :- q(X); #false: r(FUN), f(X)=FUN."],
        )

    def test_negated_body_literal_no_change(self):
        """A negated body literal without evaluable functions is unchanged."""
        self.assertEqual(
            self.unnest("p(X) :- q(X); not r(X).", {"f/1"}),
            ["p(X) :- q(X); not r(X)."],
        )

    def test_symbolic_function(self):
        """An evaluable function nested in a symbolic term is unnested.

        The parser does not produce symbolic function terms, but
        programmatically constructed ASTs may contain them.
        """
        library = self.lib.library
        position = Position(library, "<test>", 1, 1)
        location = Location(position, position)
        inner = symbol.Function(library, "f", [symbol.Number(library, 1)])
        outer = symbol.Function(library, "g", [inner, symbol.Number(library, 2)])
        term = ast.TermSymbolic(library, location, outer)
        transformer = UnnestFunctionsInLiteralsTransformer(
            library, {SymbolSignature("f", 1)}, FreshVariableGenerator()
        )
        unnested = transformer.unnest(term, False)
        self.assertEqual(str(unnested), "g(FUN,2)")
        self.assertEqual(
            [str(c) for c in transformer.pop_all_unnested_functions()],
            ["f(1)=FUN"],
        )
