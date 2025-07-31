import re
from typing import Sequence
import clingo
from clingo import solve
from clingo.symbol import Symbol, SymbolType

from fasp.symbol import FunctionSymbol


class Model:
    """
    Provides access to a model during a solve call and provides a
    `SolveContext` object to influence the running search.

    Notes
    -----
    The string representation of a model object is similar to the output of
    models by clingo using the default output.

    `Model` objects cannot be constructed from Python. Instead they are obained
    during solving (see `Control.solve`). Furthermore, the lifetime of a model
    object is limited to the scope of the callback it was passed to or until
    the search for the next model is started. They must not be stored for later
    use.
    """

    def __init__(self, model: solve.Model, prefix: str = "F"):
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
        complement
            Return the complement of the answer set w.r.t. to the atoms known
            to the grounder.

        Returns
        -------
        The selected symbols.

        Notes
        -----
        Atoms are represented using functions (`Symbol` objects), and CSP
        assignments are represented using functions with name `"$"` where the
        first argument is the name of the CSP variable and the second its
        value.
        """
        return [
            symbol
            for symbol in self.clingo_model.symbols(shown, atoms, terms, theory)
            if symbol.type != SymbolType.Function
            or not symbol.name.startswith(self.prefix)
        ]

    def function_symbols(
        self,
    ) -> Sequence[FunctionSymbol]:
        return [
            FunctionSymbol.from_symbol(symbol)
            for symbol in self.clingo_model.symbols(atoms=True)
            if symbol.type == SymbolType.Function
            and symbol.name.startswith(self.prefix)
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
        function_atoms = [str(atom) for atom in self.function_symbols()]
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
        return self.to_str(ordered=True)
