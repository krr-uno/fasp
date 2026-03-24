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
        return self.finder.identifyAggregatePattern(rewritten_nodes)

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

    ## EXTRA TESTS ##
    def test_1(self) -> None:
        program = "1 { assign(N,C1,C2) : color(C1), color(C2)} < 2 :- node(N), pos(Z)."
        expected = [FPredicate(name='assign', arity=3, arguments=(0,), values=(1, 2), condition=[])]

        self.assertFPredicateEqual(program, expected)