from clingo import ast
from clingo.core import Library
from fasp.util.ast import ELibrary
from fasp.syntax_tree.parsing.parser import parse_string

from typing import Iterable, List, Any, Optional, Set, cast


from fasp.syntax_tree.collectors import (
    collect_evaluable_functions, 
    collect_variables,
    SymbolSignature,
)

from fasp.syntax_tree._nodes import (
    FASP_AST, 
    FASP_Statement,
    AssignmentRule,
)

from fasp.util.ast import (
    StatementAST,
    AST,
)

from fasp.syntax_tree.rewritings.some_assignments import transform_choice_some_to_choice_assignment
from fasp.syntax_tree.rewritings.aggregates import normalize_assignment_aggregates
from fasp.syntax_tree.protecting import (
    protect_assignments, 
    protect_comparisons,
    restore_comparison,
    restore_assignments,
)
from clingo.ast import RewriteContext
from fasp.syntax_tree.rewritings.unnesting.rules import RuleRewriteTransformer
from fasp.syntax_tree.to_asp import NormalForm2PredicateTransformer

class FASPProgramTransformer:
    def __init__(self, elib:ELibrary, statement_asts: Iterable[AST], prefix:str = "F"):
        self.elib = elib
        self.library = elib.library

        self.statement_asts = cast(Iterable[FASP_Statement],statement_asts)
        self.prefix = prefix

        self.evaluable_functions: set[SymbolSignature] = set()
        self.pipeline = [
            self._rewrite_choice_some_wrapper,
            self._normalize_assignment_aggregates_wrapper,
            self._protect_comparisons_wrapper,
            self._protect_assignments_wrapper,
            # self._clingo_rewrite_wrapper,
            # self._restore_assignments_wrapper,
            # self._restore_comparisons_wrapper,
            # self._unnest_functions_wrapper,
            # self._to_asp_wrapper,
        ]

        # self.rule_rewriter = RuleRewriteTransformer(self.library, self.evaluable_functions)
        # self.to_asp_transformer: Optional[NormalForm2PredicateTransformer] = None

    def transform(self,*, test_pipeline=2) -> Iterable[FASP_Statement]:
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
    
    def _rewrite_choice_some_wrapper(self, statements: Iterable[FASP_Statement]) -> Iterable[FASP_Statement]:
        out = [
            transform_choice_some_to_choice_assignment(self.library, stmt) for stmt in statements
        ]
        return out

    
    def _normalize_assignment_aggregates_wrapper(self, statements: Iterable[FASP_Statement]) -> Iterable[FASP_Statement]:
        out = [
            normalize_assignment_aggregates(self.library, stmt) for stmt in statements
        ]
        return out

    def _protect_comparisons_wrapper(self, statements: Iterable[FASP_Statement]) -> Iterable[FASP_Statement]:
        return protect_comparisons(self.library, statements)
    
    def _protect_assignments_wrapper(self, statements: Iterable[FASP_Statement]) -> Iterable[FASP_Statement]:
        return protect_assignments(self.elib, statements)