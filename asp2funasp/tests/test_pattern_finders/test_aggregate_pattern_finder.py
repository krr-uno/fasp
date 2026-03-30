import textwrap
import unittest

from typing import List

from clingo import ast
from clingo.core import Library
from clingo.ast import RewriteContext

from asp2funasp.pattern_finders.aggregate_pattern_finder import AggregatePatternFinder
from asp2funasp.util.ast import AST
from asp2funasp.util.types import FPredicate, CPredicate
from asp2funasp.transformers.preprocessing import processPipelinetransformers
from tests.util import collect_statements


class AggregatePatternFinderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.finder = AggregatePatternFinder(self.lib)

    def _apply(self, program: str) -> list[FPredicate]:
        """Parse `program`, pass asts to pattern finder and return identifed FPredicates."""
        program = textwrap.dedent(program).strip()
        nodes: list[ast.StatementRule] = collect_statements(self.lib, program)

        rewritten_nodes = processPipelinetransformers(self.lib, nodes)
        self.finder.identifyAggregatePattern(rewritten_nodes)
        self.finder.identifyCountConstraintPattern(rewritten_nodes)
        return self.finder.functionalPredicates

    def assertFPredicateEqual(self, program:str, expectedFPredicates: list[FPredicate]) -> None:
        foundFPredicates = self._apply(program)
        assert len(foundFPredicates) == len(expectedFPredicates), f"Expected FPredicates of same length, got {len(foundFPredicates)} and {len(expectedFPredicates)}"
        assert all(fp in foundFPredicates for fp in expectedFPredicates), f"Expected predicates {expectedFPredicates}, found {foundFPredicates}"

    ## TESTS ##
    def test_no_aggregate_pattern(self) -> None:
        program = "2 { assign(N,C) : color(C)}  6 :- node(N), pos(Z)."
        expected = []

        self.assertFPredicateEqual(program, expected)

    def test_identifies_aggregate_pattern(self) -> None:
        program = "1 { assign(N,C) : color(C)}  1 :- node(N), pos(Z)."
        expected = [FPredicate(name='assign', arity=2, arguments=(0,), values=(1,), condition=[])]

        self.assertFPredicateEqual(program, expected)
    
    def test_count_aggregate_no_pattern(self) -> None:
        program = """
                    { assign(N,C1,C2) } :- node(N), color(C1), color(C2).
                    { assign2(N,C,X) } :- node(N), color(C), pos(X), extra(N).
                    { zero } :- node(N).

                    % Not a count aggregate.
                    :- #sum{ 1,C1,C2,N : assign(N,C1,C2)} != 1, node(N), color(C1), color(C2).

                    % Count aggregate with more than one element.
                    :- #count{ C1,C2,N : assign(N,C1,C2); C1,C2,N : q(N,C1,C2)} != 1, node(N), color(C1), color(C2).

                    % Count aggregate with only non-symbolic condition literals.
                    :- #count{ C1,C2,N : C1 = C2 } != 1, node(N), color(C1), color(C2).

                    % Guard does not match "!= 1".
                    :- #count{ C1,C2,N : assign(N,C1,C2); p(C1): p(C2)} != 1, node(N), color(C1), color(C2).
                    :- #count{ C1,C2,N : assign(N,C1,C2)} == 1, node(N), color(C1), color(C2).

                    % Count aggregate element without condition.
                    :- #count{ C1,C2,N} != 1, node(N), color(C1), color(C2).

                    % Referenced predicate has no defining rule.
                    :- #count{ C1,C2,N : undef(N,C1,C2)} != 1, node(N), color(C1), color(C2).

                    % Referenced predicate has multiple defining rules.
                    dup(N,C1,C2) :- node(N), color(C1), color(C2).
                    dup(N,C1,C2) :- node(N), color(C1), color(C2), extra(N).
                    :- #count{ C1,C2,N : dup(N,C1,C2)} != 1, node(N), color(C1), color(C2).

                    % Constraint body literals are not subset of defining rule body literals.
                    :- #count{ C,N,X : assign2(N,C,X) } != 1, node(N), pos(X), flag(N).

                    % Zero-arity predicate yields empty parameter list.
                    :- #count{ N : zero } != 1, node(N).
                """
        expected = []
        self.assertFPredicateEqual(program, expected)

    def test_count_aggregate_pattern(self) -> None:
        program = """
                    { assign(N,C,X) } :- node(N), color(C), pos(X).
                    :- #count{ C,N,X : assign(N,C,X) } != 1, node(N), pos(X).
                """
        expected = [FPredicate(name='assign', arity=3, arguments=(0, 2), values=(1,), condition=[])]
        self.assertFPredicateEqual(program, expected)

    def test_count_aggregate_zero_arity_choice_literal_no_pattern(self) -> None:
        program = """
                    zero.
                    :- #count{ 1 : zero } != 1.
                """
        expected = []
        self.assertFPredicateEqual(program, expected)

    ## EXTRA TESTS ##
    def test_1(self) -> None:
        program = "1 { assign(N,C1,C2) : color(C1), color(C2)} < 2 :- node(N), pos(Z)."
        expected = [FPredicate(name='assign', arity=3, arguments=(0,), values=(1, 2), condition=[])]

        self.assertFPredicateEqual(program, expected)