from typing import List, Tuple, TypeVar, Type

from clingo import ast
from clingo.core import Library
from asp2fasp.util.ast import AST, StatementAST
from asp2fasp.util.util import collect_statements_from_pased
T = TypeVar('T')
T_AST = TypeVar('T_AST', bound=AST)

def collect_statements(lib: Library,program:str) -> List[ast.StatementRule]:
    """Helper to collect StatementRule nodes program str."""
    nodes:List[AST] = []
    ast.parse_string(lib, program, nodes.append)
    return collect_statements_from_pased(nodes)

def find_in_ast(node:AST, typ: Type[T_AST]) -> None | T_AST:
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

def parse_and_find(lib: Library, program: str, typ: Type[T_AST]) -> None | T_AST:
    rules = collect_statements(lib, program)

    if not rules:
        return None

    return find_in_ast(rules[0], typ)

def diff_namedtuples(expected:List[T], found:List[T]) -> Tuple[List[T], List[T]]:
        missing = [e for e in expected if e not in found]
        unexpected = [f for f in found if f not in expected]
        return missing, unexpected