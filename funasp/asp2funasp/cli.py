"""Command-line interface for converting ASP programs into FUNASP source."""

import argparse
import sys
from collections.abc import Sequence
from pathlib import Path

from clingo_funasp import ast

from funasp.asp2funasp.conversion import ConversionResult, convert_statements
from funasp.ast import Statement, ast_to_str, parse_files, parse_string
from funasp.core import Library
from funasp.util.ast import ParsingException


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="asp2funasp",
        description="Detect functional predicates in ASP and emit FUNASP source.",
    )
    parser.add_argument("input", help="ASP input file, or '-' to read from stdin")
    parser.add_argument(
        "--out",
        metavar="PATH",
        help="write the converted FUNASP program to PATH instead of stdout",
    )
    return parser


def _parse_input(library: Library, input_name: str) -> list[Statement]:
    if input_name == "-":
        return parse_string(library, sys.stdin.read())
    return parse_files(library, [input_name])


def _convert(library: Library, statements: Sequence[Statement]) -> ConversionResult:
    return convert_statements(
        library.library,
        [statement.original for statement in statements],
    )


def _render(statements: Sequence[ast.Statement]) -> str:
    return "\n".join(ast_to_str(statement) for statement in statements) + "\n"


def _write(program: str, output: str | None) -> None:
    if output is None:
        sys.stdout.write(program)
        return
    Path(output).write_text(program, encoding="utf-8")


def main(argv: Sequence[str] | None = None) -> int:
    """Convert one ASP program and print or save parseable FUNASP source."""
    args = _argument_parser().parse_args(argv)
    try:
        with Library() as library:
            result = _convert(library, _parse_input(library, args.input))
            program = _render(result.converted_statements)
        _write(program, args.out)
    except ParsingException as error:
        for item in error.errors:
            sys.stderr.write(f"{item}\n")
        return 65
    except OSError as error:
        sys.stderr.write(f"asp2funasp: error: {error}\n")
        return 1
    return 0
