from clingo.ast import RewriteContext as ClingoRewriteContext

from fasp.util.ast import ELibrary


class RewriteContext:
    """
    A class for FASP Rewrite Context.

    The ELibrary instance and the prefix string are encapsulated within the RewriteContext.
    When rewriting statements or parsing files, these parameters should be accessed via
    the RewriteContext rather than passed separately.
    """

    def __init__(self, elib: ELibrary = ELibrary(), prefix: str = "F"):
        self.elib = elib
        self.ctx = ClingoRewriteContext(self.elib.library)
        self.prefix = prefix
