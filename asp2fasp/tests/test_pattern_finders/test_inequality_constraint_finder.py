import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from asp2fasp.pattern_finders.inequality_constraint_finder import InequalityConstraintFinder

from asp2fasp.util.ast import AST

from asp2fasp.util.types import FPredicate, CPredicate

class InequalityConstraintFinderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.finder = InequalityConstraintFinder()

    def _apply(self, program: str) -> list[FPredicate]:
        """Parse `program`, rewrite each rule, and return the normalized text."""
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)

        self.finder.identifyInequalityPattern(nodes)
        return self.finder.foundPredicates
    
    def assertFPredicateEqual(self, program:str, expectedFPredicates: list[FPredicate]) -> None:
        foundFPredicates = self._apply(program)
        assert len(foundFPredicates) == len(expectedFPredicates), f"Expected FPredicates of same length, got {len(foundFPredicates)} and {len(expectedFPredicates)}"
        assert all(fp in foundFPredicates for fp in expectedFPredicates), f"Expected predicates {expectedFPredicates}, found {foundFPredicates}"
    
    def test_not_constraint(self) -> None:
        program = "a."
        fPredicates = self._apply(program)
        assert len(fPredicates) == 0, f"Expected no predicates, found {len(fPredicates)}"
    
    def test_no_inequality(self) -> None:
        program = ":- pos(I,X,Y); pos(I,X1,Y1); Y > Y1."
        fPredicates = self._apply(program)
        self.assertFPredicateEqual(program, [])
    
    def test_identifies_inequality_pattern(self) -> None:
        program = """
            :- pos(I,X,Y); pos(I,X1,Y1); X1 != X.
            :- pos(I,X,Y1); pos(I,X1,Y); Y1 != Y.
        """
        expected = [    
                FPredicate(name='pos', arity=3, arguments=(0,), values=(1,), condition=[]), 
                FPredicate(name="pos",arity=3,arguments=(0,),values=(2,),condition=[])
                ]

        self.assertFPredicateEqual(program, expected)

    def test_identifies_inequality_pattern_with_conditions(self) -> None:
        program = ":- loc(A, B), loc(C, B), A != C, pred1(A)."
        expected = [FPredicate(name='loc', arity=2, arguments=(1,), values=(0,), condition=[CPredicate(name='pred1', arity=1, arguments=(0,))]),]

        self.assertFPredicateEqual(program, expected)
    
    def test_identifies_inequality_pattern_with_conditions_shared_arg(self) -> None:
        program = ":- loc(A, B), loc(C, B), A != C, pred1(A,C)."
        expected = [FPredicate(name='loc', arity=2, arguments=(1,), values=(0,), condition=[CPredicate(name='pred1', arity=2, arguments=(0,-1))]),]
        
        self.assertFPredicateEqual(program, expected)
    
    def test_inequality_pattern_with_multiple_inequalities(self) -> None:
        program = ":- unit2sensor(U,D1), unit2sensor(U,D2), unit2sensor(U,D3), D1!=D2,D2!=D3,D1!=D3."
        expected = [FPredicate(name='unit2sensor', arity=2, arguments=(0,), values=(1,), condition=[CPredicate(name='unit2sensor', arity=2, arguments=(0, -1))])]
        
        self.assertFPredicateEqual(program, expected)
    

   
    def test_inequality_pattern_with_tuple_conditions(self) -> None:
        program = ":- at(A, B, C, D), at(A, B, C', D'), (C,D) != (C',D')."
        expected = [FPredicate(name='at', arity=4, arguments=(0, 1), values=(2, 3), condition=[])]
        
        self.assertFPredicateEqual(program, expected)


    # Extra Tests
    def test_1(self) -> None:
        program = ":- at(A, B, C, D), at(A, B, C', D'), C != C', D != D'."
        expected = []
        
        self.assertFPredicateEqual(program, expected)
    
    def test_2(self) -> None:
        program = ":- at(A, B, C, D), at(A, B', C', D'), (C,D) != (C',D')."
        expected = [FPredicate(name='at', arity=4, arguments=(0,), values=(2, 3), condition=[])]

        self.assertFPredicateEqual(program, expected)

    def test_3(self) -> None:
        program = "at(A, B, C, D), at(A', B', C', D'), (C,D) != (C',D')."
        expected = []

        self.assertFPredicateEqual(program, expected)
    
    def test_4(self) -> None:
        program = ":- loc(A, B, D), loc(X, B, D), A != X."
        expected = [FPredicate(name='loc', arity=3, arguments=(1, 2), values=(0,), condition=[])]

        self.assertFPredicateEqual(program, expected)
    