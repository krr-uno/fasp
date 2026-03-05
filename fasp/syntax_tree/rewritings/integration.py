from enum import IntEnum, auto
from typing import Iterable, cast

from clingo import ast

from fasp.syntax_tree._context import RewriteContext as FASPRewriteContext
from fasp.syntax_tree._nodes import (
    AssignmentRule,
    FASP_Statement,
)
from fasp.syntax_tree.collectors import (
    collect_evaluable_functions,
)
from fasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
from fasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negated_body_literals_from_statements,
)
from fasp.syntax_tree.rewritings.protecting import (
    protect_assignments,
    protect_comparisons,
    restore_assignments,
    restore_comparisons,
)
from fasp.syntax_tree.rewritings.showf import rewrite_showf
from fasp.syntax_tree.rewritings.some_assignments import (
    transform_choice_some_to_choice_assignment,
)
from fasp.syntax_tree.rewritings.to_asp import (
    NormalForm2PredicateTransformer,
    functional_constraints,
)
from fasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer
from fasp.syntax_tree.types import SymbolSignature


class PipelineStage(IntEnum):
    SHOWF = auto()
    REWRITE_CHOICE_SOME = auto()
    NORMALIZE_ASSIGNMENT_AGGREGATES = auto()
    PROTECT_ASSIGNMENTS = auto()
    PROTECT_COMPARISONS = auto()
    CLINGO_REWRITE = auto()
    RESTORE_COMPARISONS = auto()
    RESTORE_ASSIGNMENTS = auto()
    NEGATED_LITERALS = auto()
    UNNEST_FUNCTIONS = auto()
    TO_ASP = auto()


# class Statement:

#     def __init__(self, original: FASP_Statement):
#         self.original = original
#         self.has_assignments = isinstance(original, AssignmentRule)
#         self.rewritten: list[FASP_Statement] = []


class FASPProgramTransformer:
    def __init__(
        self,
        ctx: FASPRewriteContext,
        statement_asts: Iterable[FASP_Statement],
    ):
        self.ctx = ctx
        self.lib = self.ctx.lib
        self.library = self.lib.library

        self.statement_asts = statement_asts
        self.prefix = self.ctx.prefix_function

        self.evaluable_functions: set[SymbolSignature] = set()
        self.pipeline = list(PipelineStage)

        self.PIPELINE_IMPL = {
            PipelineStage.SHOWF: self._showf_to_show_wrapper,
            PipelineStage.REWRITE_CHOICE_SOME: self._rewrite_choice_some_wrapper,
            PipelineStage.NORMALIZE_ASSIGNMENT_AGGREGATES: self._normalize_assignment_aggregates_wrapper,
            PipelineStage.PROTECT_ASSIGNMENTS: self._protect_assignments_wrapper,
            PipelineStage.PROTECT_COMPARISONS: self._protect_comparisons_wrapper,
            PipelineStage.CLINGO_REWRITE: self._clingo_rewrite_wrapper,
            PipelineStage.RESTORE_COMPARISONS: self._restore_comparisons_wrapper,
            PipelineStage.RESTORE_ASSIGNMENTS: self._restore_assignments_wrapper,
            PipelineStage.NEGATED_LITERALS: self._negated_literals_wrapper,
            PipelineStage.UNNEST_FUNCTIONS: self._unnest_functions_wrapper,
            PipelineStage.TO_ASP: self._to_asp_wrapper,
        }

    def log_info(
        self, statements: Iterable[FASP_Statement], stage: PipelineStage
    ) -> Iterable[FASP_Statement]:  # pragma: no cover
        out = list(statements)
        for s in out:
            if hasattr(s, "head") and hasattr(s, "body"):
                print(stage.name, s, type(s.head))
        return out

    def transform(
        self,
        *,
        stop_at: PipelineStage = PipelineStage.TO_ASP,
        LOG: bool = False,
    ) -> Iterable[FASP_Statement]:
        """
        Parse the program string, collect variables,
        then run the pipeline and return transformed statements.
        """
        statements = self._showf_to_show_wrapper(self.statement_asts)
        statements = self._rewrite_choice_some_wrapper(statements)
        statements = self._normalize_assignment_aggregates_wrapper(statements)
        statements = self._protect_assignments_wrapper(statements)
        statements = self._protect_comparisons_wrapper(statements)
        statements = self._clingo_rewrite_wrapper(statements)
        statements = self._restore_comparisons_wrapper(statements)
        statements = self._restore_assignments_wrapper(statements)
        statements = self._negated_literals_wrapper(statements)
        statements = self._unnest_functions_wrapper(statements)
        statements = self._to_asp_wrapper(statements)
        return statements

    def _rewrite_choice_some_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        out = [
            transform_choice_some_to_choice_assignment(self.library, stmt)
            for stmt in statements
        ]
        return out

    def _normalize_assignment_aggregates_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        out = [
            normalize_assignment_aggregates(self.library, stmt) for stmt in statements
        ]
        return out

    def _protect_assignments_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return protect_assignments(self.ctx, statements)

    def _protect_comparisons_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return protect_comparisons(self.library, statements)

    def _clingo_rewrite_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        ctx = self.ctx.ctx
        self.ctx.lib.ignore_info = True
        out = []
        errors = []
        for stmt in statements:
            assert not isinstance(stmt, AssignmentRule)
            try:
                rewritten_list = ast.rewrite_statement(ctx, stmt)
            except RuntimeError as e:
                errors.append((stmt, e))
                continue
            if rewritten_list:
                for new_stmt in rewritten_list:
                    out.append(new_stmt)
            else:
                out.append(stmt)
        self.ctx.lib.ignore_info = False
        if errors:
            raise RuntimeError("rewriting failed", errors)
        return out

    def _restore_comparisons_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:

        stmts = list(statements)

        for stmt in stmts:
            assert not isinstance(stmt, AssignmentRule)
        result = restore_comparisons(self.library, cast(Iterable[ast.Statement], stmts))
        return result

    def _restore_assignments_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return restore_assignments(
            self.lib,
            cast(Iterable[ast.Statement], statements),
            self.ctx.prefix_function,
        )

    def _negated_literals_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return rewrite_negated_body_literals_from_statements(
            self.lib, cast(Iterable[ast.Statement], statements)
        )

    def _unnest_functions_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        stmts = list(statements)
        stmts2 = list(stmts)
        self.evaluable_functions = collect_evaluable_functions(stmts2)

        transformer = RuleRewriteTransformer(self.library, self.evaluable_functions)
        out: list[FASP_Statement] = []
        for stmt in stmts:
            unnested_statement = transformer.transform_rule(stmt)
            out.append(
                cast(FASP_Statement, unnested_statement) if unnested_statement else stmt
            )
        return out

    def _to_asp_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        # Collect evaluable functions again
        self.evaluable_functions = collect_evaluable_functions(statements)

        to_asp_transformer = NormalForm2PredicateTransformer(
            self.library, self.evaluable_functions, self.prefix
        )
        out: list[ast.Statement] = []
        for stmt in statements:
            out.append(to_asp_transformer.rewrite(stmt))
        return out

    def _showf_to_show_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return [rewrite_showf(self.ctx, stmt) for stmt in statements]


def transform_to_clingo_statements(
    ctx: FASPRewriteContext,
    statement_asts: Iterable[FASP_Statement],
    *,
    stop_at: PipelineStage = PipelineStage.TO_ASP,
    LOG: bool = False,
) -> Iterable[ast.Statement]:
    """Create a FASPProgramTransformer, run transform() and return
    an iterable of clingo.ast.Statement (ast.Statement).

    This asserts that the final transformed statements are instances
    of clingo.ast.Statement and returns them as a list.
    """

    transformer = FASPProgramTransformer(ctx, statement_asts)
    transformed = transformer.transform(stop_at=stop_at, LOG=LOG)

    out: list[ast.Statement] = []
    for stmt in transformed:
        assert isinstance(
            stmt, ast.Statement
        ), f"Expected clingo.ast.Statement, got {type(stmt)}"
        out.append(stmt)
    out.extend(
        functional_constraints(
            ctx.lib.library, transformer.evaluable_functions, ctx.prefix_function
        )
    )
    return out
