from typing import Iterable, Sequence

from clingo import ast

from fasp.syntax_tree.rewritings.integration import (
    FASPProgramTransformer,
    transform_to_clingo_statements,
)
from fasp.util.ast import ELibrary

from . import rewritings
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


def parse_files(
    library: ELibrary,
    files: Sequence[str],
    prefix: str = "F",
) -> tuple[str, ast.Program]:
    """
    Parse the programs in the given files and return an abstract syntax tree for
    each statement via a callback.

    The function follows clingo's handling of files on the command line. Filename
    `"-"` is treated as stdin and if an empty list is given, then the parser will
    read from stdin.

    Parameters
    ----------
    files
        List of file names.
    """
    statements = parser.parse_files(library, files)
    transformer = FASPProgramTransformer(library, statements, prefix=prefix)
    rewritten_statements = transformer.transform()
    program = ast.Program(library.library)
    rewritten_program = ""
    for statement in rewritten_statements:
        assert not isinstance(
            statement, AssignmentRule
        ), "Assignment rules should have been rewritten by the transformer"
        program.add(statement)
        rewritten_program += str(statement) + "\n"
    return rewritten_program, program
    # return rewritings.functional2asp(library.library, statements, prefix)


### Please change to

# def rewrite_statement(
#     ctx: fasp.syntax_tree._context.RewriteContext,
#     statement: FASP_Statement,
# ) -> Iterable[ast.Statement]:

# ELibrary and prefix are part of RewriteContext

def rewrite_statement(
    ctx: ast.RewriteContext,
    statement: FASP_Statement,
    *,
    library: ELibrary | None = None,
    prefix: str = "F",
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
    lib = library if library is not None else ELibrary()
    rewritten_statements = transform_to_clingo_statements(
        lib, [statement], prefix=prefix, ctx=ctx
    )
    return rewritten_statements


def rewrite_statements(
    ctx: ast.RewriteContext,
    statements: Iterable[FASP_Statement],
    *,
    library: ELibrary | None = None,
    prefix: str = "F",
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
    lib = library if library is not None else ELibrary()
    return transform_to_clingo_statements(lib, statements, prefix=prefix, ctx=ctx)
