from enum import IntEnum, auto
from typing import Iterable, cast

from clingo.ast import Statement, rewrite_statement

from fasp.syntax_tree._context import RewriteContext as FASPRewriteContext
from fasp.syntax_tree._nodes import (
    FASP_AST,
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
from fasp.syntax_tree.rewritings.showf import showf_to_show_transformer
from fasp.syntax_tree.rewritings.some_assignments import (
    transform_choice_some_to_choice_assignment,
)
from fasp.syntax_tree.rewritings.to_asp import NormalForm2PredicateTransformer
from fasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer
from fasp.syntax_tree.types import SymbolSignature
from fasp.util.ast import (
    AST,
    ELibrary,
    StatementAST,
)


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


class FASPProgramTransformer:
    def __init__(
        self,
        ctx: FASPRewriteContext,
        statement_asts: Iterable[FASP_Statement],
        # *,
        # prefix: str = "F",
        # ctx: RewriteContext | None = None,
    ):
        self.ctx = ctx
        self.elib = self.ctx.elib
        self.library = self.elib.library

        self.statement_asts = statement_asts
        self.prefix = self.ctx.prefix

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
        parsed_statements = self.statement_asts

        # start pipeline with parsed_statements
        current: Iterable[FASP_Statement] = parsed_statements
        for stage in self.pipeline:
            current = list(self.PIPELINE_IMPL[stage](current))
            if LOG:
                self.log_info(current, stage)  # pragma: no cover
            if stage >= stop_at:
                break

        return current

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
        result = protect_assignments(self.elib, statements)

        return cast(Iterable[FASP_Statement], result)
        # return protect_assignments(self.elib, statements)

    def _protect_comparisons_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return protect_comparisons(self.library, statements)

    def _clingo_rewrite_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:

        ctx = self.ctx.ctx
        # ctx = self.ctx if self.ctx is not None else RewriteContext(self.library)

        out = []

        # clingo_statements = map(fasp_to_clingo_statement, statements)
        for stmt in statements:
            assert not isinstance(stmt, AssignmentRule)
            rewritten_list = rewrite_statement(ctx, stmt)
            # print(list(map(str, rewritten_list)))
            if rewritten_list:
                for new_stmt in rewritten_list:
                    out.append(new_stmt)
            else:
                out.append(stmt)
        return out

    def _restore_comparisons_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:

        stmts = list(statements)

        for stmt in stmts:
            assert not isinstance(stmt, AssignmentRule)
        result = restore_comparisons(self.library, cast(Iterable[StatementAST], stmts))
        return result

    def _restore_assignments_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return restore_assignments(self.elib, cast(Iterable[StatementAST], statements))

    def _negated_literals_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return rewrite_negated_body_literals_from_statements(
            self.elib, cast(Iterable[StatementAST], statements)
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
        out: list[StatementAST] = []
        for stmt in statements:
            out.append(to_asp_transformer.rewrite(stmt))
        return out

    def _showf_to_show_wrapper(
        self, statements: Iterable[FASP_Statement]
    ) -> Iterable[FASP_Statement]:
        return showf_to_show_transformer(self.ctx, statements)


def transform_to_clingo_statements(
    ctx: FASPRewriteContext,
    statement_asts: Iterable[FASP_Statement],
    *,
    stop_at: PipelineStage = PipelineStage.TO_ASP,
    LOG: bool = False,
) -> Iterable[Statement]:
    """Create a FASPProgramTransformer, run transform() and return
    an iterable of clingo.ast.Statement (ast.Statement).

    This asserts that the final transformed statements are instances
    of clingo.ast.Statement and returns them as a list.
    """

    transformer = FASPProgramTransformer(ctx, statement_asts)
    transformed = transformer.transform(stop_at=stop_at, LOG=LOG)

    out: list[Statement] = []
    for stmt in transformed:
        assert isinstance(
            stmt, Statement
        ), f"Expected clingo.ast.Statement, got {type(stmt)}"
        out.append(stmt)

    return out
