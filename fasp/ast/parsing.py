from typing import Sequence

from clingo import ast
from clingo.core import Library

from fasp.util.ast import StatementAST

from . import rewriting
from .rewriting_assigments import SymbolSignature

from .tree_sitter.parser import parse_files


def parse_files(
    library: Library,
    files: Sequence[str],
    prefix: str = "F",
) -> tuple[set[SymbolSignature], ast.Program]:
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
    # statements: list[StatementAST] = []
    # ast.parse_files(library, files, statements.append)
    statements = parse_files(library, files)
    return rewriting.functional2asp(library, statements, prefix)
