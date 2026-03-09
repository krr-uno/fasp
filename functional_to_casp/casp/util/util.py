from typing import Iterable, Tuple

from clingo import ast
from clingo.core import Library

from casp.util.ast import HeadLiteralAST, StatementAST, TermAST


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


def split_multiple_aggregate_elements(
    lib: Library, node: StatementAST
) -> Iterable[StatementAST]:
    # Only process StatementRule
    if not isinstance(node, ast.StatementRule):
        return [node]
    head = node.head
    # Only process HeadSetAggregate with no guards
    if (
        isinstance(head, ast.HeadSetAggregate)
        and head.left is None
        and head.right is None
    ):
        elements = list(head.elements)
        if len(elements) > 1:
            new_rules = []
            for elem in elements:
                # Create a new HeadSetAggregate with a single element
                new_head = ast.HeadSetAggregate(
                    lib, head.location, head.left, [elem], head.right  # single element
                )
                # Create a new StatementRule with the new head
                new_rule = ast.StatementRule(
                    lib, node.location, new_head, list(node.body)
                )
                new_rules.append(new_rule)
            return new_rules
    return [node]


def collapse_equal_bounds(
    lib: Library,
    left: ast.LeftGuard | None,
    right: ast.RightGuard | None,
) -> Tuple[ast.LeftGuard | None, ast.RightGuard | None, bool]:
    # If only right exists and is equality, move to left
    if left is None and right is not None and right.relation == ast.Relation.Equal:
        new_left = ast.LeftGuard(lib, right.term, right.relation)
        return new_left, None, True
    if left is None or right is None:
        return left, right, False
    if not same_bound_symbol(left, right):
        return left, right, False
    new_left = left.update(lib, relation=ast.Relation.Equal)
    return new_left, None, True


def same_bound_symbol(left: ast.LeftGuard, right: ast.RightGuard) -> bool:
    l_sym = getattr(left.term, "symbol", None)
    r_sym = getattr(right.term, "symbol", None)
    return l_sym is not None and r_sym is not None and l_sym == r_sym
