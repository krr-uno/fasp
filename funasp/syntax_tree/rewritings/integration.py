from typing import Callable, Final, Iterable, Sequence

from clingo import ast

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)
from funasp.syntax_tree.collectors import (
    collect_evaluable_function_signatures,
)
from funasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
from funasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negate_body_literals,
)
from funasp.syntax_tree.rewritings.protecting import (
    protect_assignment,
    protect_comparisons,
    restore_assignments,
    restore_comparisons,
)
from funasp.syntax_tree.rewritings.showf import rewrite_showf
from funasp.syntax_tree.rewritings.some_assignments import (
    rewrite_some_choices,
)
from funasp.syntax_tree.rewritings.to_asp import (
    functional_constraints,
    to_asp,
)
from funasp.syntax_tree.rewritings.unnesting.rules import (
    unnest_ast,
)


class RewritingStatement:

    def __init__(self, original: FASP_Statement):
        """Initialize the wrapper around a single original statement."""
        self.original: Final[FASP_Statement] = original
        self.has_assignments = isinstance(original, AssignmentRule)
        self._rewritten: list[FASP_Statement] | None = [original]
        self._clingo_rewritten: list[ast.Statement] | None = None

    @property
    def rewritten(self) -> Sequence[FASP_Statement]:
        """Return the current FASP-level rewritten statements."""
        if self._rewritten is not None:
            return self._rewritten
        assert self._clingo_rewritten is not None
        return self._clingo_rewritten  # pragma: no cover

    @property
    def clingo_rewritten(self) -> Sequence[ast.Statement]:
        """Return the current clingo-level rewritten statements."""
        if self._clingo_rewritten is not None:
            return self._clingo_rewritten
        raise ValueError("No clingo rewritten statements available")  # pragma: no cover

    def rewrite(
        self,
        context: RewriteContext,
        fun: Callable[[RewriteContext, FASP_Statement], FASP_Statement],
    ) -> None:
        """Apply a FASP-to-FASP rewrite function to the current statements."""
        assert self._rewritten is not None
        self._rewritten = [fun(context, stmt) for stmt in self.rewritten]

    def rewrite_to_clingo(
        self,
        context: RewriteContext,
        fun: Callable[[RewriteContext, FASP_Statement], ast.Statement],
    ) -> None:
        """Apply a FASP-to-clingo rewrite function and switch to clingo statements."""
        assert self._rewritten is not None
        self._clingo_rewritten = [fun(context, stmt) for stmt in self.rewritten]
        self._rewritten = None

    def rewrite_clingo(
        self,
        context: RewriteContext,
        fun: Callable[[RewriteContext, ast.Statement], ast.Statement],
    ) -> None:
        """Apply a clingo-to-clingo rewrite function to the current statements."""
        assert self._clingo_rewritten is not None
        self._clingo_rewritten = [fun(context, stmt) for stmt in self._clingo_rewritten]

    def rewrite_from_clingo(
        self,
        context: RewriteContext,
        fun: Callable[[RewriteContext, ast.Statement], FASP_Statement],
    ) -> None:
        """Apply a clingo-to-FASP rewrite function and switch back to FASP statements."""
        assert self._clingo_rewritten is not None
        self._rewritten = [fun(context, stmt) for stmt in self._clingo_rewritten]

    def rewrite_clingo_many(  # pragma: no cover
        self,
        context: RewriteContext,
        fun: Callable[[RewriteContext, Iterable[ast.Statement]], list[ast.Statement]],
    ) -> None:
        """Apply a rewrite function that consumes and returns multiple clingo statements."""
        assert self._clingo_rewritten is not None
        self._clingo_rewritten = fun(context, self._clingo_rewritten)


def _clingo_rewrite(context: RewriteContext, statement: RewritingStatement) -> None:
    """
    Wrapper for clingo's statement rewriting to handle errors.
    """
    statements = statement.clingo_rewritten
    ctx = context.ctx
    context.lib.ignore_info = True
    out: list[ast.Statement] = []
    errors = []
    for stmt in statements:
        assert not isinstance(stmt, AssignmentRule)
        try:
            rewritten_list = ast.rewrite_statement(ctx, stmt)
        except RuntimeError as e:
            errors.append((stmt, e))
            continue
        out.extend(rewritten_list)
    context.lib.ignore_info = False
    if errors:
        raise RuntimeError("rewriting failed", errors)
    statement._clingo_rewritten = out


def rewrite_statements(
    context: RewriteContext,
    statements: Iterable[FASP_Statement],
) -> list[ast.Statement]:
    """
    Parse the program string, collect variables,
    then run the pipeline and return transformed statements.
    """
    new_statements = [RewritingStatement(stmt) for stmt in statements]
    for stmt in new_statements:
        stmt.rewrite(context, rewrite_showf)
        stmt.rewrite(context, rewrite_some_choices)
        stmt.rewrite(context, normalize_assignment_aggregates)
        stmt.rewrite(context, rewrite_negate_body_literals)
        context.evaluable_functions |= collect_evaluable_function_signatures(
            stmt.rewritten
        )
    for stmt in new_statements:
        stmt.rewrite(context, unnest_ast)
        stmt.rewrite_to_clingo(context, protect_assignment)
        stmt.rewrite_clingo(context, protect_comparisons)
        _clingo_rewrite(context, stmt)
        stmt.rewrite_clingo(context, restore_comparisons)
        stmt.rewrite_from_clingo(context, restore_assignments)
        stmt.rewrite_to_clingo(context, to_asp)

    new_statements2 = [s for stmt in new_statements for s in stmt.clingo_rewritten]
    new_statements2.extend(functional_constraints(context))
    return new_statements2
