from clingo_funasp.ast import RewriteContext as ClingoRewriteContext

from funasp.rewriting.types import SymbolSignature
from funasp.util.ast import ELibrary


class RewriteContext:
    """
    A class for FASP Rewrite Context.

    The ELibrary instance and the prefix string are encapsulated within the RewriteContext.
    When rewriting statements or parsing files, these parameters should be accessed via
    the RewriteContext rather than passed separately.
    """

    def __init__(
        self,
        lib: ELibrary = ELibrary(),
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
