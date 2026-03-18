from typing import Final

from funasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)


class Statement:

    def __init__(self, original: FASP_Statement):
        self.original: Final[FASP_Statement] = original
        self.has_assignments = isinstance(original, AssignmentRule)
        self.rewritten: list[FASP_Statement] = [original]
