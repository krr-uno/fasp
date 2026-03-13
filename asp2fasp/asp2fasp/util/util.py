from itertools import combinations
from typing import (
    Iterable,
    List,
    Sequence,
    Tuple,
    TypeVar,
)

from clingo import ast
from clingo.core import Library
from clingo.symbol import Symbol, SymbolType

from asp2fasp.util.ast import AST, HeadLiteralAST, StatementAST, TermAST

FunctionLikeAST = ast.TermFunction | ast.TermSymbolic | ast.TermTuple | Symbol

T = TypeVar("T")


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


# def is_function(node: AST) -> TypeIs[ast.TermFunction | ast.TermSymbolic]:
#     return isinstance(node, ast.TermFunction) or (
#         isinstance(node, ast.TermSymbolic) and node.symbol.type == SymbolType.Function
#     )


def function_arguments(
    node: FunctionLikeAST,
) -> tuple[str, Sequence[ast.TermOrProjection] | Sequence[Symbol]]:

    if isinstance(node, ast.TermTuple):
        name = ""
        assert len(node.pool) == 1 and isinstance(
            node.pool[0], ast.ArgumentTuple
        ), f"Terms must be unpooled {node}"
        arguments = node.pool[0].arguments
    elif isinstance(node, ast.TermFunction):
        name = node.name
        assert len(node.pool) == 1, f"Terms must be unpooled {node}"
        arguments = node.pool[0].arguments
    else:
        if isinstance(node, ast.TermSymbolic):
            node = node.symbol
        if node.type == SymbolType.Tuple:  # pragma: no cover
            name = ""
        else:
            assert (
                node.type == SymbolType.Function
            ), f"Expected a symbol function, got {node}: {node.type}"
            name = node.name
        arguments = node.arguments
    return name, arguments


# def function_arguments_ast(
#     library: Library,
#     node: ast.TermFunction | ast.TermSymbolic,
# ) -> tuple[str, Sequence[ast.Term]]:
#     name, arguments = function_arguments(node)
#     if arguments and isinstance(arguments[0], ast.Term):
#         return name, cast(Sequence[ast.Term], arguments)
#     return name, [
#         ast.TermSymbolic(library, node.location, cast(Symbol, a)) for a in arguments
#     ]


def split_multiple_aggregate_elements(
    lib: Library, node: StatementAST
) -> Iterable[StatementAST]:
    # Only process StatementRule
    if not isinstance(node, ast.StatementRule):
        return [node]   #pragma: no cover
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


def identify_invariant_positions(
    occurrences: Sequence[Sequence[T]],
) -> List[int]:

    num_args = len(occurrences[0])
    invariant_positions = [1] * num_args  # Start with all as invariant

    for i in range(num_args):
        reference_value = occurrences[0][i]
        for occurrence in occurrences[1:]:
            if occurrence[i] != reference_value:
                invariant_positions[i] = 0
                break  # No need to check further for this index

    return invariant_positions


def get_variant_subsets(
    variant_positions: List[int],
    lists_of_elements: Sequence[Sequence[T]],
) -> List[List[Tuple[List[T], List[int]]]]:
    all_subsets: List[List[Tuple[List[T], List[int]]]] = []

    for _list in lists_of_elements:
        elements = [
            _list[i] for i in variant_positions
        ]  # Extract variant elements based on positions
        num_variants = len(elements)

        subsets: List[Tuple[List[T], List[int]]] = []
        for r in range(1, num_variants + 1):  # Generate all non-empty subsets
            for indices in combinations(range(num_variants), r):
                subset_values = [elements[i] for i in indices]  # Corresponding values
                subset_indices = [
                    variant_positions[i] for i in indices
                ]  # Corresponding original indices
                subsets.append((subset_values, subset_indices))

        all_subsets.append(subsets)

    return all_subsets
