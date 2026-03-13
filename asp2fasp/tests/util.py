from typing import List, Tuple, TypeVar

from clingo import ast
from clingo.core import Library
from asp2fasp.util.ast import AST, StatementAST

T = TypeVar('T')


def collect_statements(lib: Library,program:str) -> List[ast.StatementRule]:
    """Helper to collect StatementRule nodes program str."""
    nodes:List[AST] = []
    ast.parse_string(lib, program, nodes.append)

    stmts:List[ast.StatementRule] = []
    for n in nodes:
        if isinstance(n, ast.StatementRule):
            stmts.append(n)
    return stmts

def find_in_ast(node:AST, typ: type) -> None | AST:
    result = None

    def visitor(n:AST) -> None:
        nonlocal result

        if result is None and isinstance(n, typ):
            result = n
            return  # stop early if desired

        # recurse into children
        n.visit(visitor)

    visitor(node)
    return result

def parse_and_find(lib: Library, program: str, typ: type) -> None | AST:
    rules = collect_statements(lib, program)

    if not rules:
        return None

    return find_in_ast(rules[0], typ)

def diff_namedtuples(expected:List[T], found:List[T]) -> Tuple[List[T], List[T]]:
        missing = [e for e in expected if e not in found]
        unexpected = [f for f in found if f not in expected]
        return missing, unexpected