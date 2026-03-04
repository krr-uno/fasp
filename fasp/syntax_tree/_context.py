from clingo.ast import RewriteContext as ClingoRewriteContext

from fasp.util.ast import ELibrary


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
        prefix_variable: str = "FUN",
        prefix_variable_agg: str = "AGG",
        prefix_protect_comparison: str = "CMP",
        prefix_protect_assignment: str = "ASS",
    ):
        self.lib = lib
        self.prefix_function = prefix_function
        self.prefix_variable = prefix_variable
        self.prefix_variable_agg = prefix_variable_agg
        self.prefix_protect_comparison = prefix_protect_comparison
        self.prefix_protect_assignment = prefix_protect_assignment
        self.ctx = ClingoRewriteContext(self.lib.library)
