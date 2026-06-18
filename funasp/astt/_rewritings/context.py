from clingo_funasp.ast import RewriteContext as ClingoRewriteContext

from funasp.astt._rewritings.types import SymbolSignature
from funasp.core import Library


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
    ):
        """Initialize the RewriteContext instance."""
        self.lib = lib
        self.prefix_function = prefix_function
        self.ctx = ClingoRewriteContext(self.lib.library)
        self.ctx.project_anonymous = True
        self.intensional_functions: set[SymbolSignature] = (
            set(intensional_functions) if intensional_functions is not None else set()
        )
