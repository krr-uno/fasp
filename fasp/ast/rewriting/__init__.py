from functools import singledispatchmethod
from itertools import chain
from typing import AbstractSet, Any, Iterable, cast

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.symbol import Number

from fasp.ast import (
    FASP_AST,
    FASP_AST_T,
    AssignmentRule,
    FASP_Statement,
    HeadSimpleAssignment,
)
from fasp.ast.protecting import (  # protect_comparisons,; restore_comparisons,
    COMPARISON_NAME,
)
from fasp.ast.rewriting.aggregates import (
    normalize_assignment_aggregates,
)
from fasp.ast.rewriting.collectors import (
    SymbolSignature,
    collect_evaluable_functions,
)
from fasp.ast.rewriting.normal_form_to_predicate import (
    NormalForm2PredicateTransformer,
    functional_constraints,
)
from fasp.util.ast import (
    BodyLiteralAST,
    StatementAST,
    create_body_literal,
    create_literal,
    function_arguments,
    function_arguments_ast,
    is_function,
)

# def normalize_ast(
#     library: Library, statements: Iterable[FASP_Statement]
# ) -> Iterable[FASP_Statement]:
#     rewrite_context = ast.RewriteContext(library)
#     return restore_comparisons(
#         library,
#         chain.from_iterable(
#             ast.rewrite_statement(rewrite_context, stmt)
#             for stmt in protect_comparisons(library, statements)
#         ),
#     )


def _functional2asp(
    library: Library, statements: Iterable[FASP_Statement], prefix: str = "F"
) -> tuple[set[SymbolSignature], list[StatementAST]]:
    """
    Transform a program in functional normal form into a regular program.

    Args:
        program (Iterable[ast.AST]): The program to transform.

    Returns:
        Iterable[ast.AST]: The transformed program.
    """
    statements = [normalize_assignment_aggregates(library, stm) for stm in statements]
    evaluable_functions = collect_evaluable_functions(statements)
    transformer = NormalForm2PredicateTransformer(library, evaluable_functions, prefix)
    return (
        evaluable_functions,
        list(
            chain(
                (transformer.rewrite(stm) for stm in statements),
                functional_constraints(library, evaluable_functions, prefix),
            )
        ),
    )


def functional2asp(
    library: Library, statements: Iterable[FASP_Statement], prefix: str = "F"
) -> tuple[set[SymbolSignature], ast.Program]:
    """
    Transform a program in functional normal form into a regular program.

    Args:
        program (Iterable[ast.AST]): The program to transform.

    Returns:
        Iterable[ast.AST]: The transformed program.
    """
    # statements = normalize_ast(library, statements)
    evaluable_functions, statements = _functional2asp(library, statements, prefix)
    program = ast.Program(library)
    for statement in statements:
        program.add(statement)
    return evaluable_functions, program
