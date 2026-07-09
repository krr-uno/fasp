"""
Collection of intensional function signatures from the prefixed representation.

The ``clingo_funasp`` parser encodes an assignment ``f(t1,...,tn) := v`` as
the atom ``Ff(t1,...,tn,v)``: the function name gets the prefix and the
assigned value is appended as the last argument. Consequently, a prefixed
head atom of arity ``n+1`` declares the intensional function ``f/n``.

Collection must run after ``#some`` and aggregate assignments have been
normalized (see ``rewrite_statements.py``), so the only places where assignments
appear are:

- ``HeadSimpleLiteral`` atoms (``Ff(t,v) :- B.``),
- ``HeadSetAggregate`` element literals (``{ Ff(t,v) } :- B.``),
- ``HeadAggregate`` element literals (``#count{ ...: Ff(t,v): ... }.``).
"""

from clingo_funasp import ast, symbol

from funasp.ast import PARSER_PREFIX
from funasp.util.types import SymbolSignature


def _signatures_from_literal(prefix: str, literal: ast.Literal) -> set[SymbolSignature]:
    """Collect the signature declared by a prefixed head atom, if any."""
    if not isinstance(literal, ast.LiteralSymbolic):
        return set()
    atom = literal.atom
    if isinstance(atom, ast.TermFunction) and atom.name.startswith(prefix):
        name = atom.name[len(prefix) :]
        return {
            SymbolSignature(name, len(arguments.arguments) - 1)
            for arguments in atom.pool
        }
    if (
        isinstance(atom, ast.TermSymbolic)
        and atom.symbol.type == symbol.SymbolType.Function
        and atom.symbol.name.startswith(prefix)
    ):
        name = atom.symbol.name[len(prefix) :]
        return {SymbolSignature(name, len(atom.symbol.arguments) - 1)}
    return set()


def collect_shown_function_signatures(
    statement: ast.Statement,
) -> set[SymbolSignature]:
    """
    Collect the intensional function signatures declared by ``#showf``.

    The parser rewrites ``#showf f/n.`` into ``#show Ff/n+1.``; a prefixed
    show-signature name unambiguously marks an intensional function even
    when the function is never assigned.
    """
    if isinstance(statement, ast.StatementShowSignature) and statement.name.startswith(
        PARSER_PREFIX
    ):
        name = statement.name[len(PARSER_PREFIX) :]
        return {SymbolSignature(name, statement.arity - 1)}
    return set()


def collect_intensional_function_signatures(
    statement: ast.Statement,
) -> set[SymbolSignature]:
    """
    Collect the intensional function signatures declared by a statement head.
    """
    if not isinstance(statement, ast.StatementRule):
        return set()
    prefix = PARSER_PREFIX
    head = statement.head
    signatures: set[SymbolSignature] = set()
    if isinstance(head, ast.HeadSimpleLiteral):
        signatures |= _signatures_from_literal(prefix, head.literal)
    elif isinstance(head, ast.HeadSetAggregate | ast.HeadAggregate):
        for element in head.elements:
            signatures |= _signatures_from_literal(prefix, element.literal)
    return signatures
