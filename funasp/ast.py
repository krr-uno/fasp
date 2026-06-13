"""
Parsing wrappers and AST iteration helpers.

``parse_string``/``parse_files`` wrap the callback-based ``clingo_funasp.ast``
parser into functions that return a ``list[Statement]`` (each parsed clingo
statement wrapped in a :class:`Statement`) and convert clingo parsing errors
into :class:`~funasp.util.ast.ParsingException` carrying
:class:`~funasp.util.ast.SyntacticError` locations. ``transform_iterable``
applies a transformer across an iterable, preserving unchanged elements.
"""

import re
from dataclasses import dataclass, field
from typing import Callable, Iterable, Sequence

from clingo_funasp import ast
from clingo_funasp.core import Library, Location, MessageType, Position

from funasp import core
from funasp.util.ast import ParsingException, SyntacticError

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


@dataclass
class Statement:
    """A wrapper for clingo_funasp.ast.Statement that keeps track to the original statement for better error reporting during rewriting."""

    lib: Library
    original: ast.Statement
    rewritten: list[ast.Statement] = field(init=False)

    def __post_init__(self) -> None:
        self.rewritten = [self.original]

    def __str__(self) -> str:
        # Imported lazily: ``funasp.printer`` pulls in ``funasp.rewriting``,
        # which depends on this module, so a top-level import would cycle.
        from funasp.printer import ast_to_str

        return ast_to_str(self.original)


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


def transform_iterable[T, R](
    library: Library, iterable: Iterable[T], fun: Callable[[Library, T], R | None]
) -> list[T | R] | None:
    """
    Apply a function to each element of an iterable.
    If all elements are transformed to None, return None. Otherwise, return an iterable of the transformed elements, where elements that were transformed to None are replaced by the original element.

    Args:
        iterable (Iterable[T]): The input iterable of elements of type T.
        fun (Callable[[Library, T], R | None]): A function that takes a Library and an element of type T and returns a transformed element of type R or None.

    Returns:
        list[T | R] | None: A list of transformed elements, or None if all elements were transformed to None.
    """
    result: list[T | R] = []
    all_none = True
    for element in iterable:
        new_element = fun(library, element)
        if new_element is not None:
            all_none = False
            result.append(new_element)
        else:
            result.append(element)
    if all_none:
        return None
    return result
