from typing import Sequence

from clingo_funasp import solve
from clingo_funasp.symbol import Symbol, SymbolType

from funasp.symbol import FunctionSymbol
from funasp.util.types import AUXILIARY_PREDICATE_PREFIXES


def _is_hidden_auxiliary_symbol(symbol: Symbol) -> bool:
    return symbol.type == SymbolType.Function and symbol.name.startswith(
        AUXILIARY_PREDICATE_PREFIXES
    )


def _is_function_assignment_symbol(symbol: Symbol, prefix: str) -> bool:
    """Return whether the symbol encodes an intensional function assignment.

    Auxiliary predicate names never start with the function prefix (see
    ``RewriteContext._auxiliary_prefix``), so every atom starting with the
    prefix is a function assignment — even when the prefix itself starts
    with an auxiliary prefix such as ``RD``.
    """
    return symbol.type == SymbolType.Function and symbol.name.startswith(prefix)


def _is_internal_symbol(symbol: Symbol, prefix: str) -> bool:
    """Return whether the symbol is internal to the FASP encoding."""
    return _is_function_assignment_symbol(
        symbol, prefix
    ) or _is_hidden_auxiliary_symbol(symbol)


class Model:
    """FASP-aware view of a clingo model.

    Encoded function predicates are exposed as :class:`FunctionSymbol` values,
    and pipeline-generated auxiliary predicates are hidden from user output.
    """

    def __init__(self, model: solve.Model, prefix: str = "F"):
        """Initialize the Model instance."""
        self.clingo_model = model
        self.prefix = prefix

    def predicate_symbols(
        self,
        shown: bool = False,
        atoms: bool = False,
        terms: bool = False,
        theory: bool = False,
    ) -> Sequence[Symbol]:
        """
        Return the list of atoms, terms, or CSP assignments in the model.

        Parameters
        ----------
        atoms
            Select all atoms in the model (independent of `#show` statements).
        terms
            Select all terms displayed with `#show` statements in the model.
        shown
            Select all atoms and terms as outputted by clingo.
        theory
            Select atoms added with `Model.extend`.
        Returns
        -------
        The selected symbols.
        """
        return [
            symbol
            for symbol in self.clingo_model.symbols(shown, atoms, terms, theory)
            if not _is_internal_symbol(symbol, self.prefix)
        ]

    def function_symbols(
        self,
        shown: bool = False,
        atoms: bool = False,
        terms: bool = False,
        theory: bool = False,
    ) -> Sequence[FunctionSymbol]:
        """Return the shown function assignments extracted from the underlying model."""
        return [
            FunctionSymbol.from_symbol(symbol, prefix_len=len(self.prefix))
            for symbol in self.clingo_model.symbols(shown, atoms, terms, theory)
            if _is_function_assignment_symbol(symbol, self.prefix)
        ]

    def to_str(self, *, ordered: bool = False) -> str:
        """
        Return a string representation of the model.

        Parameters
        ----------
        ordered
            If True, the atoms in the model are sorted before printing.

        Returns
        -------
        A string representation of the model.
        """
        predicate_atoms = [str(atom) for atom in self.predicate_symbols(shown=True)]
        function_atoms = [str(atom) for atom in self.function_symbols(shown=True)]
        if ordered:
            predicate_atoms.sort()
            function_atoms.sort()
        predicate_str = " ".join(predicate_atoms)
        function_str = " ".join(function_atoms)
        if not predicate_str:
            return function_str
        if not function_str:
            return predicate_str
        return f"{predicate_str}\n{function_str}"

    def __repr__(self) -> str:
        """Return the developer-facing string representation of this Model."""
        return self.to_str(ordered=True)
