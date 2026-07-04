import unittest

from clingo_funasp.core import Library
from clingo_funasp.symbol import Function, Number

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
