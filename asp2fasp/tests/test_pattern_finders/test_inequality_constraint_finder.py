import textwrap
import unittest

from collections import namedtuple

from clingo import ast
from clingo.core import Library

from asp2fasp.pattern_finders.inequality_constraint_finder import InequalityConstraintFinder

from asp2fasp.util.ast import AST


FPredicate = namedtuple(
    "FPredicate", ["name", "arity", "arguments", "values", "condition"]
)
CPredicate = namedtuple("CPredicate", ["name", "arity", "arguments"])

class InequalityConstraintFinderTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()
        self.finder = InequalityConstraintFinder()

    def _apply(self, program: str) -> str:
        """Parse `program`, rewrite each rule, and return the normalized text."""
        program = textwrap.dedent(program).strip()
        nodes: list[AST] = []
        ast.parse_string(self.lib, program, nodes.append)

        self.finder.identifyInequalityPattern(nodes)
        return "test"

    def test_not_constraint(self) -> None:
        program = "a."
        self._apply(program)
        assert len(self.finder.foundPredicates) == 0, f"Expected no predicates, found {len(self.finder.foundPredicates)}"
    def test_no_inequality(self) -> None:
        program = ":- pos(I,X,Y); pos(I,X1,Y1); Y > Y1."
        self._apply(program)
        assert len(self.finder.foundPredicates) == 0, f"Expected no predicates, found {len(self.finder.foundPredicates)}"
    
    def test_identifies_inequality_pattern(self) -> None:
        program = """
            :- pos(I,X,Y); pos(I,X1,Y1); X1 != X.
            :- pos(I,X,Y1); pos(I,X1,Y); Y1 != Y.
        """
        self._apply(program)
        
        expected = [FPredicate(
            name="pos",
            arity=3,
            arguments=(0,),
            values=(1,),
            condition=[]
        ), FPredicate(
            name="pos",
            arity=3,
            arguments=(0,),
            values=(2,),
            condition=[]
        )]

        assert len(self.finder.foundPredicates) == len(expected), f"Expected {len(expected)} predicates, found {len(self.finder.foundPredicates)}"
        assert all(fp in self.finder.foundPredicates for fp in expected), f"Expected predicates {expected}, found {self.finder.foundPredicates}"