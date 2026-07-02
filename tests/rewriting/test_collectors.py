"""
Unit tests for ``funasp.ast._rewritings.collectors`` paths that the integrated
pipeline cannot reach: the parser encodes assignment heads as ``TermFunction``
atoms, so a prefixed head represented as a ground ``TermSymbolic`` function only
arises from AST built by hand.
"""

import unittest

from clingo_funasp import ast, symbol

from funasp.ast import PARSER_PREFIX, parse_string
from funasp.ast._rewritings.collectors import (
    collect_intensional_function_signatures,
)
from funasp.core import Library
from funasp.util.types import SymbolSignature


class TestCollectSignatures(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()
        self.clib = self.lib.library

    def _symbolic_head_rule(self, name: str, arguments: list[int]) -> ast.Statement:
        """Build a fact whose head atom is a ground symbolic ``name`` function."""
        statement = parse_string(self.lib, "g.")[1].original
        head = statement.head
        literal = head.literal
        function = symbol.Function(
            self.clib,
            name,
            [symbol.Number(self.clib, argument) for argument in arguments],
            True,
        )
        atom = literal.atom.update(self.clib, symbol=function)
        literal = literal.update(self.clib, atom=atom)
        head = head.update(self.clib, literal=literal)
        return statement.update(self.clib, head=head)

    def test_symbolic_prefixed_head(self):
        """A prefixed ground symbolic head declares its intensional signature."""
        statement = self._symbolic_head_rule(PARSER_PREFIX + "f", [1, 2])
        self.assertEqual(str(statement), f"{PARSER_PREFIX}f(1,2).")
        self.assertEqual(
            collect_intensional_function_signatures(statement),
            {SymbolSignature("f", 1)},
        )

    def test_symbolic_non_prefixed_head(self):
        """A ground symbolic head without the prefix declares no signature."""
        statement = self._symbolic_head_rule("g", [1, 2])
        self.assertEqual(collect_intensional_function_signatures(statement), set())


if __name__ == "__main__":
    unittest.main()
