import textwrap
import unittest
from typing import List, Tuple, Type, TypeVar

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp import convert_statements
from funasp.asp2funasp.util.types import FRelation
from funasp.asp2funasp.util.util import collect_statements_from_parsed
from funasp.ast import ast_to_str, parse_string
from funasp.control import Control
from funasp.core import Library as FunaspLibrary
from funasp.util.ast import AST

T = TypeVar("T")
T_AST = TypeVar("T_AST", bound=AST)


def collect_statements(lib: Library, program: str) -> List[ast.StatementRule]:
    """Helper to collect StatementRule nodes program str."""
    nodes: List[AST] = []
    ast.parse_string(lib, program, nodes.append)
    return collect_statements_from_parsed(nodes)


def collect_all_statements(lib: Library, program: str) -> List[ast.Statement]:
    """Parse all source statements, retaining directives but not parser setup."""
    nodes: List[ast.Statement] = []
    ast.parse_string(lib, program, nodes.append)
    return [node for node in nodes if not isinstance(node, ast.StatementProgram)]


# def collect_statements_funasp(lib: ELibrary,program:str) -> List[FASP_Statement]:
#     """Helper to collect StatementRule | AssignmentRule nodes program str."""
#     nodes:List[AST] = []
#     nodes = parse_string(lib, program)
#     return collect_statements_from_parsed_funasp(nodes)


def find_in_ast(node: AST, typ: Type[T_AST]) -> None | T_AST:
    result = None

    def visitor(n: AST) -> None:
        nonlocal result

        if result is None and isinstance(n, typ):
            result = n
            return  # stop early if desired

        # recurse into children
        n.visit(visitor)

    visitor(node)
    return result


def parse_and_find(lib: Library, program: str, typ: Type[T_AST]) -> None | T_AST:
    rules = collect_statements(lib, program)

    if not rules:
        return None

    return find_in_ast(rules[0], typ)


def diff_namedtuples(expected: List[T], found: List[T]) -> Tuple[List[T], List[T]]:
    missing = [e for e in expected if e not in found]
    unexpected = [f for f in found if f not in expected]
    return missing, unexpected


class ConversionTestCase(unittest.TestCase):
    """Keep parser and solver setup consistent across conversion tests."""

    model_prefix = "F"

    def setUp(self) -> None:
        self.maxDiff = None

    def _convert(self, program: str) -> tuple[str, tuple[FRelation, ...]]:
        """Return rendered FUNASP and the accepted functional relations."""
        with FunaspLibrary() as library:
            result = convert_statements(
                library.library,
                [
                    statement.original
                    for statement in parse_string(
                        library, textwrap.dedent(program).strip()
                    )
                ],
            )
            return (
                "\n".join(ast_to_str(s) for s in result.converted_statements),
                result.accepted_relations,
            )

    def _convert_source(self, program: str) -> str:
        """Return just the rendered source when metadata is not under test."""
        return self._convert(program)[0]

    def _models(
        self, program: str, convert: bool = False, prefix: str | None = None
    ) -> set[str]:
        """Enumerate shown models, optionally enabling ASP conversion."""
        with FunaspLibrary(logger=lambda *_: None) as library:
            control = Control(
                library,
                ["0"],
                asp2funasp=convert,
                prefix=prefix if prefix is not None else self.model_prefix,
            )
            control.parse_string(textwrap.dedent(program).strip())
            control.ground()
            return {str(model) for model in control.solve()}

    def assertConversionEqual(
        self, program: str, expected_program: str
    ) -> tuple[str, tuple[FRelation, ...]]:
        """Assert exact rendered source, retaining metadata for further checks."""
        converted, relations = self._convert(program)
        self.assertEqual(converted, textwrap.dedent(expected_program).strip())
        return converted, relations
