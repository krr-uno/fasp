from typing import Optional, Sequence

from clingo import Logger, ast
from clingo.ast import AST

from .syntax_checking import SymbolSignature
from . import rewriting

from clingo.ast import parse_files


def parse_files(
    files: Sequence[str],
    logger: Optional[Logger] = None,
    message_limit: int = 20,
    prefix: str = "F",
) -> tuple[set[SymbolSignature], list[ast.AST]]:
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
    callback
        Callable taking an ast as argument.
    logger
        Function to intercept messages normally printed to standard error.
    message_limit
        The maximum number of messages passed to the logger.
    """
    statements = []
    ast.parse_files(files, statements.append, None, logger, message_limit)
    return rewriting.functional2asp(statements, prefix)
