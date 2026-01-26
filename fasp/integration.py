from enum import IntEnum
from typing import Iterable, cast

from clingo.ast import RewriteContext, rewrite_statement

from fasp.syntax_tree._nodes import (
    FASP_AST,
    AssignmentRule,
    FASP_Statement,
)
from fasp.syntax_tree.collectors import (
    SymbolSignature,
    collect_evaluable_functions,
)
from fasp.syntax_tree.protecting import (
    protect_assignments,
    protect_comparisons,
    restore_assignments,
    restore_comparisons,
)
from fasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
from fasp.syntax_tree.rewritings.negated_literals import (
    rewrite_negated_body_literals_from_statements,
)
from fasp.syntax_tree.rewritings.some_assignments import (
    transform_choice_some_to_choice_assignment,
)
from fasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer
from fasp.syntax_tree.to_asp import NormalForm2PredicateTransformer
from fasp.util.ast import (
    AST,
    ELibrary,
    StatementAST,
)


class PipelineStage(IntEnum):
    REWRITE_CHOICE_SOME = 1
    NORMALIZE_ASSIGNMENT_AGGREGATES = 2
    PROTECT_ASSIGNMENTS = 3
    PROTECT_COMPARISONS = 4
    CLINGO_REWRITE = 5
    RESTORE_COMPARISONS = 6
    RESTORE_ASSIGNMENTS = 7
    NEGATED_LITERALS = 8
    UNNEST_FUNCTIONS = 9
    TO_ASP = 10


class FASPProgramTransformer:
    def __init__(
        self, elib: ELibrary, statement_asts: Iterable[AST], prefix: str = "F"
    ):
        self.elib = elib
        self.library = elib.library

        self.statement_asts = cast(Iterable[FASP_Statement], statement_asts)
        self.prefix = prefix

        self.evaluable_functions: set[SymbolSignature] = set()
        self.pipeline = [
            PipelineStage.REWRITE_CHOICE_SOME,
            PipelineStage.NORMALIZE_ASSIGNMENT_AGGREGATES,
            PipelineStage.PROTECT_ASSIGNMENTS,
            PipelineStage.PROTECT_COMPARISONS,
            PipelineStage.CLINGO_REWRITE,
            PipelineStage.RESTORE_COMPARISONS,
            PipelineStage.RESTORE_ASSIGNMENTS,
            PipelineStage.NEGATED_LITERALS,
            PipelineStage.UNNEST_FUNCTIONS,
            PipelineStage.TO_ASP,
        ]

        self.PIPELINE_IMPL = {
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

        ctx = RewriteContext(self.library)

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
