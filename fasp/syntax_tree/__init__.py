from typing import Iterable, Sequence

from clingo import ast

from fasp.syntax_tree.rewritings.integration import (
    FASPProgramTransformer,
    transform_to_clingo_statements,
)
from fasp.util.ast import ELibrary

from . import rewritings
from ._context import RewriteContext
from ._nodes import (
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
)
from .collectors import SymbolSignature
from .parsing import parser

_ALL__ = [
    "FASP_AST",
    "FASP_AST_T",
    "AssignmentAggregateElement",
    "AssignmentAST",
    "AssignmentRule",
    "ChoiceAssignment",
    "FASP_Statement",
    "HeadAggregateAssignment",
    "HeadAssignment",
    "HeadSimpleAssignment",
    "SymbolSignature",
    "parse_files",
]


### Please change to

# def rewrite_statement(
#     ctx: fasp.syntax_tree._context.RewriteContext,
#     statement: FASP_Statement,
# ) -> Iterable[ast.Statement]:

# ELibrary and prefix are part of RewriteContext


def rewrite_statement(
    ctx: RewriteContext,
    statement: FASP_Statement,
) -> Iterable[ast.Statement]:
    """
    Rewrite a statement in the FASP AST to a list of statements in the clingo
    AST.

    Parameters
    ----------
    ctx
        The rewrite context.
    statement
        The statement to rewrite.
    """
    rewritten_statements = transform_to_clingo_statements(ctx, [statement])
    return rewritten_statements


def rewrite_statements(
    ctx: RewriteContext,
    statements: Iterable[FASP_Statement],
) -> Iterable[ast.Statement]:
    """Rewrite an iterable of FASP statements to clingo AST statements.

    Parameters
    ----------
    ctx
        The rewrite context.
    statements
        Iterable of FASP_Statement to rewrite.
    library
        The ELibrary to use. If None, a new ELibrary() is created.
    prefix
        Prefix to use for generated predicate names.
    """
    return transform_to_clingo_statements(ctx, statements)
