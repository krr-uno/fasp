import unittest

from clingo_funasp.core import Library
from clingo_funasp.symbol import Function, Number

from funasp.solve import _is_hidden_auxiliary_symbol
from funasp.symbol import FunctionSymbol


class TestFunctionSymbol(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.lib = Library()

    def test_from_symbol_uses_configured_prefix_length(self):
        """Function symbols are trimmed using the configured prefix length."""
        symbol = Function(
            self.lib,
            "Funf",
            [Number(self.lib, 1), Number(self.lib, 2)],
        )

        function = FunctionSymbol.from_symbol(symbol, prefix_len=3)

        self.assertEqual(function.name, "f")
        self.assertEqual(list(function.arguments), [Number(self.lib, 1)])
        self.assertEqual(function.value, Number(self.lib, 2))
        self.assertEqual(str(function), "f(1)=2")

    def test_hidden_auxiliary_symbols_require_uppercase_prefix(self):
        """Only uppercase auxiliary predicates are hidden from model output."""
        self.assertTrue(_is_hidden_auxiliary_symbol(Function(self.lib, "RD1", [])))
        self.assertTrue(_is_hidden_auxiliary_symbol(Function(self.lib, "AD1", [])))
        self.assertFalse(_is_hidden_auxiliary_symbol(Function(self.lib, "pD1", [])))
