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
    collect_variables,
)
from fasp.syntax_tree.parsing.parser import parse_string
from fasp.syntax_tree.protecting import (
    protect_assignments,
    protect_comparisons,
    restore_assignments,
    restore_comparisons,
)
from fasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
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
            self._rewrite_choice_some_wrapper,
            self._normalize_assignment_aggregates_wrapper,
            self._protect_assignments_wrapper,
            self._protect_comparisons_wrapper,
            self._clingo_rewrite_wrapper,
            self._restore_comparisons_wrapper,
            self._restore_assignments_wrapper,
            # self._unnest_functions_wrapper,
            # self._to_asp_wrapper,
        ]

        # self.rule_rewriter = RuleRewriteTransformer(self.library, self.evaluable_functions)
        # self.to_asp_transformer: Optional[NormalForm2PredicateTransformer] = None

    def transform(self, *, test_pipeline: int = 2) -> Iterable[FASP_Statement]:
        """
        Parse the program string, collect variables,
        then run the pipeline and return transformed statements.
        """
        parsed_statements = self.statement_asts

        self.program_variables = set()
        for stm in parsed_statements:
            self.program_variables.update(collect_variables(stm))

        # start pipeline with parsed_statements
        current: Iterable[FASP_Statement] = parsed_statements
        count = 1
        for stage in self.pipeline:
            current = stage(current)
            count += 1
            if count > test_pipeline:
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
