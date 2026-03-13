from typing import List, Tuple

import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from asp2fasp.rewriting import FunctionalPredicateFinder
from asp2fasp.util.ast import AST
from asp2fasp.util.types import FPredicate, CPredicate, FRelation

from tests.util import collect_statements, diff_namedtuples

class FunctionalPredicateFinderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.finder = FunctionalPredicateFinder(self.lib)

    def _apply(self, program: str) -> Tuple[List[FPredicate], List[FRelation]]:
        """Parse `program`, pass asts to pattern finder and return identifed FPredicates."""
        program = textwrap.dedent(program).strip()
        nodes: list[ast.StatementRule] = collect_statements(self.lib, program)

        return self.finder.processProgram(nodes)

    def assertFPredicateEqual(self, program:str, expected: Tuple[List[FPredicate], List[FRelation]]) -> None:
        foundFPredicates, foundFRelations = self._apply(program)
        expectedFPredicates, expectedFRelations = expected

        missing_fp, unexpected_fp = diff_namedtuples(expectedFPredicates, foundFPredicates)
        missing_fr, unexpected_fr = diff_namedtuples(expectedFRelations, foundFRelations)

        assert not missing_fp and not unexpected_fp, (
            f"\nFPredicate mismatch:\n"
            f"Missing: {missing_fp}\n"
            f"Unexpected: {unexpected_fp}\n"
            f"Expected: {expectedFPredicates}\n"
            f"Found: {foundFPredicates}"
        )

        assert not missing_fr and not unexpected_fr, (
            f"\nFRelation mismatch:\n"
            f"Missing: {missing_fr}\n"
            f"Unexpected: {unexpected_fr}\n"
            f"Expected: {expectedFRelations}\n"
            f"Found: {foundFRelations}"
        )

    ## TESTS ##
    def test_no_constraint(self) -> None:
        program = "a."
        
        self.assertFPredicateEqual(program, ([],[]))
    
    def test_no_inequality(self) -> None:
        program = ":- pos(I,X,Y); pos(I,X1,Y1); Y > Y1."
        
        self.assertFPredicateEqual(program, ([],[]))
    
    def test_inequality(self) -> None:
        program = ":- pos(I,X,Y); pos(I,X1,Y1); Y != Y1."
        
        self.assertFPredicateEqual(program, ([FPredicate(name='pos', arity=3, arguments=(0,), values=(2,), condition=[])],[]))

    def test_identifies_inequality_pattern(self) -> None:
        program = """
            :- pos(I,X,Y); pos(I,X1,Y1); X1 != X.
            :- pos(I,X,Y1); pos(I,X1,Y); Y1 != Y.
        """
        expected = (
                [FPredicate(name='pos', arity=3, arguments=(0,), values=(1,), condition=[]), 
                FPredicate(name="pos",arity=3,arguments=(0,),values=(2,),condition=[])],
                [FRelation(name='pos', arity=3, arguments=(0,), values=[(1,),(2,)])]
                )

        self.assertFPredicateEqual(program, expected)
