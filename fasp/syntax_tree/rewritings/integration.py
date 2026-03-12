from enum import IntEnum, auto
from typing import Iterable

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
    to_asp,
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
    ) -> Iterable[FASP_Statement]:
        """
        Parse the program string, collect variables,
        then run the pipeline and return transformed statements.
        """
        statements = [rewrite_showf(self.ctx, stmt) for stmt in self.statement_asts]
        statements = [
            transform_choice_some_to_choice_assignment(self.library, stmt)
            for stmt in statements
        ]
        statements = [
            normalize_assignment_aggregates(self.library, stmt) for stmt in statements
        ]
        statements = protect_assignments(self.ctx, statements)
        statements = protect_comparisons(self.library, statements)
        statements = self._clingo_rewrite_wrapper(statements)
        statements = restore_comparisons(self.library, statements)
        statements = restore_assignments(
            self.lib,
            statements,
            self.ctx.prefix_function,
        )
        statements = rewrite_negated_body_literals_from_statements(self.lib, statements)
        self.evaluable_functions = collect_evaluable_functions(statements)
        transformer = RuleRewriteTransformer(self.library, self.evaluable_functions)
        statements = [transformer.transform_rule(stmt) or stmt for stmt in statements]
        statements = to_asp(
            self.library, statements, self.evaluable_functions, self.prefix
        )
        return statements

    def _clingo_rewrite_wrapper(
        self, statements: Iterable[ast.Statement]
    ) -> Iterable[ast.Statement]:
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


def transform_to_clingo_statements(
    ctx: FASPRewriteContext,
    statement_asts: Iterable[FASP_Statement],
) -> Iterable[ast.Statement]:
    """Create a FASPProgramTransformer, run transform() and return
    an iterable of clingo.ast.Statement (ast.Statement).

    This asserts that the final transformed statements are instances
    of clingo.ast.Statement and returns them as a list.
    """

    transformer = FASPProgramTransformer(ctx, statement_asts)
    transformed = transformer.transform()

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
