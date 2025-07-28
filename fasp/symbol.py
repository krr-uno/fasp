from dataclasses import dataclass
from typing import Self, Sequence

from clingo import Symbol
import clingo


@dataclass(frozen=True, slots=True, order=True)
class FunctionSymbol:

    name: str
    value: Symbol
    arguments: Sequence[Symbol] = ()

    def __repr__(self) -> str:
        """
        Return the string representation of the function symbol.
        """
        args = ", ".join(map(str, self.arguments))
        if args:
            args = f"({args})"
        return f"{self.name}{args}={str(self.value)}"

    @classmethod
    def from_symbol(cls, symbol: Symbol, prefix_len: int = 1) -> Self:
        """
        Convert a clingo symbol to a function symbol.

        Parameters
        ----------
        symbol : clingo.Symbol
            The clingo symbol to convert.

        Returns
        -------
        FunctionSymbol
            The converted function symbol.
        """
        assert len(symbol.name) > prefix_len
        assert symbol.type == clingo.SymbolType.Function
        assert len(symbol.arguments) >= 1

        name = symbol.arguments.name[prefix_len:]
        args = symbol.arguments[:-1]
        return_value = symbol.arguments[-1]
        return cls(name, return_value, args)
