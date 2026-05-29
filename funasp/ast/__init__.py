from typing import Iterable

from clingo import ast

from funasp.ast._context import RewriteContext
from funasp.ast._nodes import (
    FASP_AST,
    FASP_AST_T,
    AssignmentAggregateElement,
    AssignmentAST,
    AssignmentRule,
    ChoiceAssignment,
    FASP_Statement,
    HeadAssignment,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
    ShowFDirective,
)
from funasp.ast.rewritings.integration import (
    rewrite_statements,
)
from funasp.ast.types import SymbolSignature

__all__ = [
    "FASP_AST",
    "FASP_AST_T",
    "AssignmentAggregateElement",
    "AssignmentAST",
    "AssignmentRule",
    "ChoiceAssignment",
    "FASP_Statement",
    "HeadAssignmentAggregate",
    "HeadAssignment",
    "HeadSimpleAssignment",
    "SymbolSignature",
    "ShowFDirective",
    "rewrite_statements",
]
