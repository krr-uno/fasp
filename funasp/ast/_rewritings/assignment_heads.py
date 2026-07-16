"""
Detection of parser-generated aggregate assignment heads.

The parser encodes both ordinary aggregate assignments and ``#some``
assignments as a ``HeadAggregate`` whose left guard is an equality against a
prefixed function term. The two rewriting steps share this detection.
"""

from clingo_funasp import ast


def prefixed_assignment_head(
    statement: ast.Statement, prefix: str
) -> tuple[ast.StatementRule, ast.HeadAggregate, ast.TermFunction] | None:
    """Return the matched rule, its assignment head, and the target term.

    Matches rules whose head is a ``HeadAggregate`` with an equality left
    guard over a function term carrying ``prefix``; the target term holds the
    (possibly pooled) assignment arguments.
    """
    if not isinstance(statement, ast.StatementRule) or not isinstance(
        head := statement.head, ast.HeadAggregate
    ):
        return None
    left = head.left
    if (
        left is None
        or not isinstance(left.term, ast.TermFunction)
        or not left.term.name.startswith(prefix)
    ):
        return None
    assert left.relation == ast.Relation.Equal
    assert head.right is None
    return statement, head, left.term
