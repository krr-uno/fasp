"""
Generation of uniqueness constraints.

For every intensional function ``f/n`` collected during rewriting, a constraint

    :- Ff(X0,...,Xn-1,_), 1 < #count{ V : Ff(X0,...,Xn-1,V) }.

is appended, enforcing that ``f`` takes at most one value per argument tuple.
"""

from typing import Iterable

from clingo_funasp import ast
from clingo_funasp.core import Library, Location, Position
from clingo_funasp.symbol import Number

from funasp.ast._rewritings.context import RewriteContext
from funasp.util.ast import create_body_literal, create_literal
from funasp.util.types import SymbolSignature


def _functional_constraint(
    library: Library, function: SymbolSignature, prefix: str = "F"
) -> ast.StatementRule:
    """
    Generate a functional constraint for a single intensional function.

    Args:
        function (SymbolSignature): The intensional function to generate the constraint for.
        prefix (str): The prefix to use for the function name.

    Returns:
        ast.AST: The functional constraint as an AST node.
    """
    position = Position(library, "<functional>", 0, 0)
    location = Location(position, position)
    anonymous_variable = ast.TermVariable(library, location, "_")
    return_variable = ast.TermVariable(library, location, "V")
    if function.arity == 0:
        args1 = [anonymous_variable]
        args2 = [return_variable]
    else:
        args1 = [
            ast.TermVariable(library, location, f"X{i}") for i in range(function.arity)
        ]
        args2 = list(args1)
        args1.append(anonymous_variable)
        args2.append(return_variable)
    name = f"{prefix}{function.name}"
    args1tuple = ast.ArgumentTuple(library, args1)
    args2tuple = ast.ArgumentTuple(library, args2)
    lit1 = create_body_literal(
        library,
        ast.TermFunction(library, location, name, [args1tuple]),
    )
    lit2 = create_literal(
        library, ast.TermFunction(library, location, name, [args2tuple])
    )
    agg = ast.BodyAggregate(
        library,
        location,
        ast.Sign.NoSign,
        ast.LeftGuard(
            library,
            ast.TermSymbolic(library, location, Number(library, 1)),
            ast.Relation.Less,
        ),
        ast.AggregateFunction.Count,
        [
            ast.BodyAggregateElement(library, location, [return_variable], [lit2]),
        ],
        None,
    )
    head = ast.HeadSimpleLiteral(
        library, ast.LiteralBoolean(library, location, ast.Sign.NoSign, False)
    )
    return ast.StatementRule(library, location, head, [lit1, agg])


def functional_constraints(
    context: RewriteContext,
) -> Iterable[ast.StatementRule]:
    """
    Generate functional constraints for intensional functions.

    Args:
        context (RewriteContext): The rewrite context with the collected
            intensional functions.

    Returns:
        list[ast.AST]: A list of constraints for the functional normal form.
    """
    return (
        _functional_constraint(context.lib.library, fun, context.prefix_function)
        for fun in sorted(context.intensional_functions)
    )
