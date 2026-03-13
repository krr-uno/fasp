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
    def test_not_constraint(self) -> None:
        program = "a."
        fPredicates,fRelation = self._apply(program)
        assert len(fPredicates) == 0, f"Expected no predicates, found {len(fPredicates)}"
        assert len(fRelation) == 0, f"Expected no relations, found {len(fRelation)}"
    
    def test_no_inequality(self) -> None:
        program = ":- pos(I,X,Y); pos(I,X1,Y1); Y > Y1."
        fPredicates,fRelation = self._apply(program)
        self.assertFPredicateEqual(program, ([],[]))
    
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

    # def test_identifies_inequality_pattern_with_conditions(self) -> None:
    #     program = ":- loc(A, B), loc(C, B), A != C, pred1(A)."
    #     expected = [FPredicate(name='loc', arity=2, arguments=(1,), values=(0,), condition=[CPredicate(name='pred1', arity=1, arguments=(0,))]),]

    #     self.assertFPredicateEqual(program, expected)
    
    # def test_identifies_inequality_pattern_with_conditions_shared_arg(self) -> None:
    #     program = ":- loc(A, B), loc(C, B), A != C, pred1(A,C)."
    #     expected = [FPredicate(name='loc', arity=2, arguments=(1,), values=(0,), condition=[CPredicate(name='pred1', arity=2, arguments=(0,-1))]),]
        
    #     self.assertFPredicateEqual(program, expected)
    
    # def test_inequality_pattern_with_multiple_inequalities(self) -> None:
    #     program = ":- unit2sensor(U,D1), unit2sensor(U,D2), unit2sensor(U,D3), D1!=D2,D2!=D3,D1!=D3."
    #     expected = [FPredicate(name='unit2sensor', arity=2, arguments=(0,), values=(1,), condition=[CPredicate(name='unit2sensor', arity=2, arguments=(0, -1))])]
        
    #     self.assertFPredicateEqual(program, expected)
    
    # def test_inequality_pattern_with_tuple_conditions(self) -> None:
    #     program = ":- at(A, B, C, D), at(A, B, C', D'), (C,D) != (C',D')."
    #     expected = [FPredicate(name='at', arity=4, arguments=(0, 1), values=(2, 3), condition=[])]
        
    #     self.assertFPredicateEqual(program, expected)

    # ## Extra Tests (Not needed for coverage) ## 
    # def test_1(self) -> None:
    #     program = ":- at(A, B, C, D), at(A, B, C', D'), C != C', D != D'."
    #     expected = []
        
    #     self.assertFPredicateEqual(program, expected)
    
    # def test_2(self) -> None:
    #     program = ":- at(A, B, C, D), at(A, B', C', D'), (C,D) != (C',D')."
    #     expected = [FPredicate(name='at', arity=4, arguments=(0,), values=(2, 3), condition=[])]

    #     self.assertFPredicateEqual(program, expected)

    # def test_3(self) -> None:
    #     program = "at(A, B, C, D), at(A', B', C', D'), (C,D) != (C',D')."
    #     expected = []

    #     self.assertFPredicateEqual(program, expected)
    
    # def test_4(self) -> None:
    #     program = ":- loc(A, B, D), loc(X, B, D), A != X."
    #     expected = [FPredicate(name='loc', arity=3, arguments=(1, 2), values=(0,), condition=[])]

    #     self.assertFPredicateEqual(program, expected)
    