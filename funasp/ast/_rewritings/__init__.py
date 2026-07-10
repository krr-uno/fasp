"""
Orchestration of the rewriting pipeline over the prefixed representation.

The ``clingo_funasp`` parser desugars FASP assignments at parse time into
plain clingo AST with prefixed atoms (``a := 1`` becomes ``Fa(1)``). This
pipeline turns that purely syntactic encoding into a semantically correct
ASP program, mirroring the old FASP-node pipeline:

0. Collect all predicates occurring in the program, including ``#show``
   signatures (used for prefix validation and to pick fresh auxiliary
   predicate names).
1. Validate the configured function prefix: an empty prefix is always
   rejected; prefixes colliding with used predicate names are rejected
   unless collision checks are explicitly ignored.
2. Per statement: rewrite ``#some`` assignments, normalize aggregate
   assignments, collect intensional function signatures, and lift negated
   condition literals into auxiliary rules (kept alongside the statement
   they originate from).
3. Per statement: move negated head literals to the body, rewrite negated
   body literals, lift doubly negated body literals over intensional
   functions into auxiliary rules, unnest intensional functions, rename
   parser prefixes to the configured prefix, rewrite functional equalities
   into prefixed literals, run clingo's statement rewriting, and restore the
   prefixed literals whose unpooled arity is not intensional.
4. Append the uniqueness constraints.

Finally, the library is told which predicate signatures encode intensional
functions (assigned or ``#showf``-declared), so that log messages are
normalized only for those predicates.
"""

from functools import partial
from typing import Iterable

from clingo_funasp import ast

from funasp.ast import Statement
from funasp.util.ast import RewritingException, SemanticError
from funasp.util.collectors import collect_predicates
from funasp.util.types import SymbolSignature

from .aggregates import rewrite_assignment_aggregates
from .collectors import (
    collect_intensional_function_signatures,
    collect_shown_function_signatures,
)
from .comparisons import prefix_comparisons
from .constraints import functional_constraints
from .context import RewriteContext
from .negated_literals import (
    rewrite_double_negated_body_literals,
    rewrite_negated_body_literals,
    rewrite_negated_condition_literals,
    rewrite_negated_head_literals,
)
from .prefixes import rename_prefixes
from .restore import restore_non_intensional_functions
from .some_assignments import rewrite_some_assignments
from .unnesting import unnest_statement
from .validation import validate_intensional_function_positions


def _prefix_collisions(context: RewriteContext) -> list[SymbolSignature]:
    """Return predicate signatures whose names collide with the function prefix."""
    prefix = context.prefix_function
    # Uppercase-initial names cannot be user-written predicates (the parser
    # reads them as variables); at this point they can only be the parser's
    # own F-prefixed function encodings, which are never collisions.
    return sorted(
        signature
        for signature in context.predicates
        if signature.name.startswith(prefix) and not signature.name[:1].isupper()
    )


def _validate_prefix_collisions(
    context: RewriteContext, statements: list[Statement]
) -> None:
    """Reject function prefixes that collide with predicates in the program."""
    if not statements:
        return
    location = statements[0].original.location
    if not context.prefix_function:
        raise RewritingException(
            [SemanticError(location, "function prefix must not be empty")]
        )
    if context.ignore_prefix_collisions:
        return
    collisions = _prefix_collisions(context)
    if not collisions:
        return
    collision_list = ", ".join(str(signature) for signature in collisions)
    raise RewritingException(
        [
            SemanticError(
                location,
                f"function prefix {context.prefix_function!r} collides with "
                f"predicate(s): {collision_list}",
            )
        ]
    )


def clingo_rewrite_wrapper(
    context: RewriteContext, original: Statement, statement: ast.Statement
) -> list[ast.Statement]:
    """
    Wrapper for clingo's statement rewriting to handle errors.
    """
    try:
        context.lib.processing_statement(str(original))
        return list(ast.rewrite_statement(context.ctx, statement))
    except RuntimeError as e:
        raise RuntimeError("rewriting failed", [(statement, e)])
    finally:
        context.lib.clear_processing_statement()


def rewrite_statements(
    context: RewriteContext,
    statements: Iterable[Statement],
) -> list[Statement]:
    """
    Run the pipeline over parsed statements and return transformed statements.

    Each input :class:`~funasp.ast.Statement` keeps its ``original`` and has its
    ``rewritten`` list filled with the clingo statements it expands to. The
    uniqueness constraints are appended as additional wrapped statements.
    """
    statements = list(statements)
    shown_functions: set[SymbolSignature] = set()
    for stmt in statements:
        for clingo_stmt in stmt.rewritten:
            context.predicates |= collect_predicates(clingo_stmt)
            shown_functions |= collect_shown_function_signatures(clingo_stmt)
    _validate_prefix_collisions(context, statements)
    new_statements: list[Statement] = []
    for stmt in statements:
        stmt.rewrite(partial(rewrite_some_assignments, context))
        stmt.rewrite(partial(rewrite_assignment_aggregates, context))
        for clingo_stmt in stmt.rewritten:
            context.intensional_functions |= collect_intensional_function_signatures(
                clingo_stmt
            )
        new_statements.append(stmt)
    for stmt in new_statements:
        stmt.rewrite(partial(validate_intensional_function_positions, context))
        stmt.rewrite(partial(rewrite_negated_condition_literals, context))
        stmt.rewrite(partial(rewrite_negated_head_literals, context))
        stmt.rewrite(partial(rewrite_negated_body_literals, context))
        stmt.rewrite(partial(rewrite_double_negated_body_literals, context))
        stmt.rewrite(partial(unnest_statement, context))
        stmt.rewrite(partial(rename_prefixes, context))
        stmt.rewrite(partial(prefix_comparisons, context))
        stmt.rewrite(partial(clingo_rewrite_wrapper, context, stmt))
        stmt.rewrite(partial(restore_non_intensional_functions, context))

    for constraint in functional_constraints(context):
        new_statements.append(Statement(context.lib.library, constraint))
    context.lib.function_predicates |= {
        SymbolSignature(f"{context.prefix_function}{name}", arity + 1)
        for name, arity in context.intensional_functions | shown_functions
    }
    return new_statements
