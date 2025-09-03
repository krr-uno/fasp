from typing import Sequence
import unittest

from clingo import ast
from clingo.core import Location, Position, Library

from fasp.util.ast import SyntacticCheckVisitor, SyntacticError, TermAST
from fasp.parsing import parser

class TestParseAssignment(unittest.TestCase):

    def setUp(self):
        self.messages = []
        self.lib = Library(logger=lambda t, msg: self.messages.append((t, msg)))

    def test_parse_assignment_right_hand_side(self):
        code = "g(Y)"
        right, pc, lt = parser._parse_assignment_right(self.lib, code)
        self.assertEqual(str(right), "g(Y)")
        self.assertIsInstance(right, TermAST)
        self.assertEqual(pc, [])
        self.assertEqual(lt, [])
        code = "a"
        right, pc, lt = parser._parse_assignment_right(self.lib, code)
        self.assertEqual(str(right), "a")
        self.assertIsInstance(right, TermAST)
        self.assertEqual(pc, [])
        self.assertEqual(lt, [])
        code = "#sum { X: p(X), q(X) }"
        right, pc, lt = parser._parse_assignment_right(self.lib, code)
        self.assertEqual(str(right), "#sum { X: p(X), q(X) }")
        self.assertIsInstance(right, ast.BodyAggregate)
        self.assertEqual(pc, [])
        self.assertEqual(lt, [])
        code = "g(Y), h(Y)"
        with self.assertRaises(ValueError) as cm:
             parser._parse_assignment_right(self.lib, code)
        self.assertEqual(
            str(cm.exception), f"Expected exactly one term or aggregate in assignment right-hand side of an assignment, found {code}")
        code = "X = Y"
        with self.assertRaises(ValueError) as cm:
             parser._parse_assignment_right(self.lib, code)
        self.assertEqual(
            str(cm.exception), f"Expected exactly one term or aggregate in assignment right-hand side of an assignment, found {code}")
        code = "#sum { X: p(X): q(X) }"
        with self.assertRaises(ValueError) as cm:
             parser._parse_assignment_right(self.lib, code)
        self.assertEqual(
            str(cm.exception), f"Expected exactly one term or aggregate in assignment right-hand side of an assignment, found {code}")
        self.assertEqual(len(self.messages), 1)