from typing import Iterable, Tuple

from clingo import ast

from casp.util.ast import HeadLiteralAST, TermAST


# Utility to negate a Relation
def negate_operator(op: ast.Relation) -> ast.Relation:
    # clingo.core.Relation
    rel = ast.Relation
    if op == rel.Equal:
        return rel.NotEqual
    elif op == rel.NotEqual:
        return rel.Equal
    elif op == rel.Less:
        return rel.GreaterEqual
    elif op == rel.LessEqual:
        return rel.Greater
    elif op == rel.Greater:
        return rel.LessEqual
    elif op == rel.GreaterEqual:
        return rel.Less
    else:
        raise ValueError(f"Unknown relation: {op}")  # pragma: no cover


def is_constraint(rule_head: HeadLiteralAST) -> bool:
    """
    Given a rule head, returns true if it is a constraint head (#false), else returns False.
    """
    if (
        isinstance(rule_head, ast.HeadSimpleLiteral)
        and isinstance(rule_head.literal, ast.LiteralBoolean)
        and rule_head.literal.sign == ast.Sign.NoSign
        and rule_head.literal.value == False
    ):
        return True
    return False


def extract_comparison_terms(
    comp: ast.LiteralComparison,
) -> Tuple[Iterable[TermAST], ast.Relation, Iterable[TermAST]]:
    """
    Extracts (lhs_terms, op, rhs_terms) from a LiteralComparison
    """
    # NOTE: Need to check if this is the correct way to extract terms
    lhs_terms = [comp.left]
    op = None
    rhs_terms = []
    for guard in comp.right:
        op = guard.relation
        rhs_terms.append(guard.term)
    assert op is not None
    return (lhs_terms, op, rhs_terms)

    # if op is not None:
    #     return (lhs_terms, op, rhs_terms)
    # return None
