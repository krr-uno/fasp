import unittest
from clingo import ast
from clingo.core import Library

def parse_string(library: Library, string: str):
    l = []
    ast.parse_string(library, string, l.append)
    return l

class TestRewrite(unittest.TestCase):

    def setUp(self):
        self.library = Library()

    def test_1(self):
        l = parse_string(self.library, """\
            p(1 %* block comment *%). f(1 %* another block comment *%). % comment
            p(1) % line comment
                         :- q(1).""")
        self.assertEqual(len(l), 8)
         
        self.assertEqual(str(l[0]), "%* block comment *%")
        self.assertEqual(str(l[1]), "#program base.")
        self.assertEqual(str(l[2]), "p(1).")
        self.assertEqual(str(l[3]), "%* another block comment *%")
        self.assertEqual(str(l[4]), "f(1).")
        self.assertEqual(str(l[5]), "% comment")
        self.assertEqual(str(l[6]), "% line comment")
        self.assertEqual(str(l[7]), "p(1) :- q(1).")