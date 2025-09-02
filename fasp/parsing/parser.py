

from clingo.core import Library
from clingo import ast

from fasp.parsing.lexer import UnParsedClingoCode
from fasp.util.ast import StatementAST


def _parse_clingo_block(library: Library, block: UnParsedClingoCode) -> list[StatementAST]:
    statements = []
    ast.parse_string(ast.Library(), block.source, statements.append)
    return statements