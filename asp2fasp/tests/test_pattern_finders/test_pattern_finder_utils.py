from typing import List

import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from asp2fasp.util.types import FPredicate
from tests.util import collect_statements

from asp2fasp.pattern_finders.pattern_finder_utils import split_program


class PatternFinderUtilsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    def _split_program(self, program:str, *, PRINT:bool = False) -> tuple[List[ast.StatementRule], dict[str, List[ast.StatementRule]]]:
        rules = collect_statements(self.lib,program)
        constraints, definitions = split_program(rules)

        if PRINT:
            for constraint in constraints:
                print(constraint)
            for pred,rules in definitions.items():
                print(f"{pred}, : {list(map(str,rules))}")
        return constraints, definitions
    def assertCorrectSplit(
        self,
        program: str,
        expected_constraints: List[str],
        expected_definitions: dict[str, List[str]],
        *,
        PRINT:bool = False
    ) -> None:
        constraints, definitions = self._split_program(program, PRINT=PRINT)

        # convert AST → str
        constraint_strs = [str(c) for c in constraints]
        definition_strs = {
            pred: [str(r) for r in rules]
            for pred, rules in definitions.items()
        }

        # compare constraints (order independent)
        self.assertEqual(
            set(constraint_strs),
            set(expected_constraints),
            f"Constraints mismatch.\nActual: {constraint_strs}\nExpected: {expected_constraints}"
        )

        # compare predicate keys
        self.assertEqual(
            set(definition_strs.keys()),
            set(expected_definitions.keys()),
            f"Predicate keys mismatch.\nActual: {definition_strs.keys()}\nExpected: {expected_definitions.keys()}"
        )

        # compare rules per predicate
        for pred in expected_definitions:
            self.assertEqual(
                set(definition_strs[pred]),
                set(expected_definitions[pred]),
                f"Definition mismatch for predicate '{pred}'.\n"
                f"Actual:   {definition_strs[pred]}\nExpected: {expected_definitions[pred]}"
            )

    ## TESTS ##
    def test_split_1(self) -> None:
        program = """
                    keep(X) :- vertex(X), not delete(X).
                    delete(X) :- vertex(X), not keep(X).
                    :- delete(X), vertex(Y), not keep(Y), X != Y.
                    kept_edge(V1, V2) :- keep(V1), keep(V2), edge(V1, V2).
                    reachable(X, Y) :- kept_edge(X, Y).
                    reachable(X, Z) :- delete(D), edge(X, D), reachable(X, Y), reachable(Y, Z).
                    :- delete(D), edge(V1, D), edge(D, V2), not reachable(V1, V2).
                    blue(N) :- keep(N), not red(N), not green(N).
                    red(N) :- keep(N), not blue(N), not green(N).
                    green(N) :- keep(N), not red(N), not blue(N).
                    :- kept_edge(N1,N2), blue(N1), blue(N2).
                    :- kept_edge(N1,N2), red(N1), red(N2).
                    :- kept_edge(N1,N2), green(N1), green(N2).
                
                """
        expected_constraints = [
                    ' :- delete(X); vertex(Y); not keep(Y); X!=Y.',
                    ' :- delete(D); edge(V1,D); edge(D,V2); not reachable(V1,V2).',
                    ' :- kept_edge(N1,N2); blue(N1); blue(N2).',
                    ' :- kept_edge(N1,N2); red(N1); red(N2).',
                    ' :- kept_edge(N1,N2); green(N1); green(N2).'
                ]
        expected_definitions = {
                    'keep/1': ['keep(X) :- vertex(X); not delete(X).'], 
                    'delete/1': ['delete(X) :- vertex(X); not keep(X).'], 
                    'kept_edge/2': ['kept_edge(V1,V2) :- keep(V1); keep(V2); edge(V1,V2).'], 
                    'reachable/2': ['reachable(X,Y) :- kept_edge(X,Y).',
                                    'reachable(X,Z) :- delete(D); edge(X,D); reachable(X,Y); reachable(Y,Z).'], 
                    'blue/1': ['blue(N) :- keep(N); not red(N); not green(N).'], 
                    'red/1': ['red(N) :- keep(N); not blue(N); not green(N).'], 
                    'green/1': ['green(N) :- keep(N); not red(N); not blue(N).']
                }

        self.assertCorrectSplit(program, expected_constraints, expected_definitions)

    def test_split_2(self) -> None:
        program = """
                    % Guess colours.
                    chosenColour(N,C) | notChosenColour(N,C) :- node(N), colour(C).

                    % At least one color per node.
                    :- node(X), not colored(X).
                    colored(X) :- chosenColour(X,Fv1).

                    % Only one color per node.
                    :- chosenColour(N,C1), chosenColour(N,C2), C1!=C2. 

                    % No two adjacent nodes have the same colour. 
                    :- link(X,Y),  X<Y, chosenColour(X,C), chosenColour(Y,C).
                """
        expected_constraints = [
                    " :- node(X); not colored(X).",
                    " :- chosenColour(N,C1); chosenColour(N,C2); C1!=C2.",
                    " :- link(X,Y); X<Y; chosenColour(X,C); chosenColour(Y,C)."
                ]
        expected_definitions = {
                    'chosenColour/2': ['chosenColour(N,C); notChosenColour(N,C) :- node(N); colour(C).'],
                    'notChosenColour/2': ['chosenColour(N,C); notChosenColour(N,C) :- node(N); colour(C).'],
                    'colored/1': ['colored(X) :- chosenColour(X,Fv1).'],
                }
        self.assertCorrectSplit(program, expected_constraints, expected_definitions)

    def test_split_head_aggregates(self) -> None:
        program = """
                    a | b(X), r(a), c(a,b) :- b(X).
                    #count { X: a(1) } == 1 :- a(X).
                    { a(X,Y): b(X); p(X): r(X) } < 2 :- c(X,Y), b(X), r(a), c(a,b).
                """
        expected_constraints = []
        expected_definitions = {
                    'b/1': ['a; b(X); r(a); c(a,b) :- b(X).'],
                    'r/1': ['a; b(X); r(a); c(a,b) :- b(X).'],
                    'c/2': ['a; b(X); r(a); c(a,b) :- b(X).'],
                    'a/1': ['#count { X: a(1) } = 1 :- a(X).'],
                    'a/2': ['{ a(X,Y): b(X); p(X): r(X) } < 2 :- c(X,Y); b(X); r(a); c(a,b).'],
                    'p/1': ['{ a(X,Y): b(X); p(X): r(X) } < 2 :- c(X,Y); b(X); r(a); c(a,b).'],
                }
        self.assertCorrectSplit(program, expected_constraints, expected_definitions)
