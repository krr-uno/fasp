from typing import Sequence

from clingo import ast

from fasp.util.ast import ELibrary

from . import rewriting
from .rewriting.collectors import SymbolSignature
from .tree_sitter import parser


def parse_files(
    library: ELibrary,
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
    statements = parser.parse_files(library, files)
    return rewriting.functional2asp(library.library, statements, prefix)
