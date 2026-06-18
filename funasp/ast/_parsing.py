import re
from typing import Sequence

from clingo_funasp import ast
from clingo_funasp.core import Library, Location, MessageType, Position

from funasp import core
from funasp.util.ast import ParsingException, SyntacticError

from ._core import Statement

# Errors for strings use an angle-bracketed name (`<string>:1:2-3: error: ...`)
# while errors for files use the plain file name (`file.lp:1:2-3: error: ...`).
_PARSING_ERROR_RE = r"(?:<(?P<bracketed>.*?)>|(?P<file>.*?)):(?P<line>\d+):(?P<col_start>\d+)-(?P<col_end>\d+): error: (?P<msg>.*)"
_PARSING_ERROR_PATTERN = re.compile(_PARSING_ERROR_RE)


def _process_error(
    library: Library, message: tuple[MessageType, str]
) -> SyntacticError:
    """Convert a clingo parsing error message tuple into a SyntacticError."""
    match = _PARSING_ERROR_PATTERN.match(message[1])
    if not match:  # pragma: no cover
        position = Position(library, "<unknown>", 0, 0)
        location = Location(position, position)
        msg = message[1]
    else:
        file = match["bracketed"] if match["bracketed"] is not None else match["file"]
        msg = match["msg"]
        start = Position(library, file, int(match["line"]), int(match["col_start"]))
        end = Position(library, file, int(match["line"]), int(match["col_end"]))
        location = Location(start, end)
    return SyntacticError(
        location,
        msg,
    )


def parse_string(library: core.Library, code: str) -> list[Statement]:
    """
    Parse a string into a list of AST statements.

    Args:
        library (Library): The library to use for parsing.
        code (str): The code string to parse.

    Returns:
        list[Statement]: The list of parsed statements.

    Raises:
        Raises ParsingError if parsing fails.
    """
    parsed: list[Statement] = []
    # The error messages are stored to restore them after parsing
    # The library is set to have no error messages during parsing
    # This avoids mixing errors from previous operations with parsing errors
    # This errors will be returned in the ParsingError if parsing fails
    saved_errors = library.error_messages
    library.error_messages = []
    try:
        ast.parse_string(
            library.library,
            code,
            lambda stmt: parsed.append(Statement(library.library, stmt)),
        )
    except RuntimeError as e:
        if str(e) != "parsing failed":  # pragma: no cover
            raise e
        raise ParsingException(
            [_process_error(library.library, error) for error in library.error_messages]
        )
    finally:
        library.error_messages = saved_errors
    return parsed


def parse_files(library: core.Library, files: Sequence[str]) -> list[Statement]:
    """
    Parse the given files into a list of AST statements.

    Args:
        library (Library): The library to use for parsing.
        files (Sequence[str]): The paths of the files to parse.

    Returns:
        list[Statement]: The list of parsed statements.

    Raises:
        Raises ParsingError if parsing fails.
    """
    parsed: list[Statement] = []
    # The error messages are stored to restore them after parsing
    # The library is set to have no error messages during parsing
    # This avoids mixing errors from previous operations with parsing errors
    # This errors will be returned in the ParsingError if parsing fails
    saved_errors = library.error_messages
    library.error_messages = []
    try:
        ast.parse_files(
            library.library,
            files,
            lambda stmt: parsed.append(Statement(library.library, stmt)),
        )
    except RuntimeError as e:
        if str(e) != "parsing failed":  # pragma: no cover
            raise e
        raise ParsingException(
            [_process_error(library.library, error) for error in library.error_messages]
        )
    finally:
        library.error_messages = saved_errors
    return parsed
