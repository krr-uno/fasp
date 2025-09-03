

from clingo.core import Library
from clingo import ast

from fasp.parsing.lexer import UnParsedAssignmentRule, UnParsedClingoCode
from fasp.util.ast import StatementAST, TermAST


def _parse_clingo_block(library: Library, block: UnParsedClingoCode) -> list[StatementAST]:
    statements = []
    ast.parse_string(ast.Library(), block.source, statements.append)
    return statements



def _parse_assignment_head(library: Library, code: str, assignment_pos: int) -> StatementAST:


# def _parse_assignment_rule(library: Library, rule: UnParsedAssignmentRule) -> StatementAST:
#     statement = StatementAST()
#     ast.parse_string(ast.Library(), rule.source, statement.append)
#     return statement