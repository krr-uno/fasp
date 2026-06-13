from clingo_funasp.ast import RewriteContext as ClingoRewriteContext

from funasp.core import Library
from funasp.rewriting.types import SymbolSignature


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
        evaluable_functions: set[SymbolSignature] | None = None,
    ):
        """Initialize the RewriteContext instance."""
        self.lib = lib
        self.prefix_function = prefix_function
        self.ctx = ClingoRewriteContext(self.lib.library)
        self.ctx.project_anonymous = True
        self.evaluable_functions: set[SymbolSignature] = (
            set(evaluable_functions) if evaluable_functions is not None else set()
        )
