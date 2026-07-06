from clingo_funasp.ast import RewriteContext as ClingoRewriteContext

from funasp.core import Library
from funasp.util.types import SymbolSignature

_AUXILIARY_PREDICATE_PREFIXES = ("RD", "AD")


class RewriteContext:
    """
    A class for FASP Rewrite Context.

    The Library instance and the prefix string are encapsulated within the RewriteContext.
    When rewriting statements or parsing files, these parameters should be accessed via
    the RewriteContext rather than passed separately.
    """

    def __init__(
        self,
        lib: Library = Library(),
        prefix_function: str = "F",
        *,
        intensional_functions: set[SymbolSignature] | None = None,
        predicates: set[SymbolSignature] | None = None,
    ):
        """Initialize the RewriteContext instance."""
        self.lib = lib
        self.prefix_function = prefix_function
        self.lib.prefix_function = prefix_function
        self.ctx = ClingoRewriteContext(self.lib.library)
        self.ctx.project_anonymous = True
        self.intensional_functions: set[SymbolSignature] = (
            set(intensional_functions) if intensional_functions is not None else set()
        )
        self.predicates: set[SymbolSignature] = (
            set(predicates) if predicates is not None else set()
        )
        self._aux_counter = 0

    def _auxiliary_prefix(self, prefix: str) -> str:
        """Return an auxiliary prefix that cannot be mistaken for a function prefix."""
        if not prefix.startswith(self.prefix_function):
            return prefix
        for candidate in _AUXILIARY_PREDICATE_PREFIXES:
            if not candidate.startswith(self.prefix_function):
                return candidate
        raise ValueError("could not find an auxiliary predicate prefix")

    def fresh_predicate_name(self, prefix: str = "RD") -> str:
        """Return a fresh predicate name not colliding with program predicates."""
        prefix = self._auxiliary_prefix(prefix)
        used_names = {signature.name for signature in self.predicates}
        while True:
            self._aux_counter += 1
            candidate = f"{prefix}{self._aux_counter}"
            if candidate not in used_names:
                return candidate
