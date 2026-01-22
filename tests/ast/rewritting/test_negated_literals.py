import unittest

from clingo import ast

from fasp.syntax_tree.parsing.parser import parse_string
from fasp.util.ast import ELibrary
from fasp.syntax_tree.rewritings.negated_literals import remove_negated_literals_from_head_in_statements

class TestNegatedLiteralsTransformer(unittest.TestCase):
    def setUp(self):
        self.lib = ELibrary()

    def apply_negated_literals_transformer(self, program: str):
        """
        Runs the negated literal transformer on the statement ASTs 
        for a given string program
        
        :param program: The input ASP program str
        :type program: str
        """

        stmts = parse_string(self.lib, program)

        # skip #program directive if present
        stmts = (
            stmts[1:] if stmts and isinstance(stmts[0], ast.StatementProgram) else stmts
        )

        new_stmts = remove_negated_literals_from_head_in_statements(stmts)

        new_stmts_str = []
        for stmt in new_stmts:
            new_stmts_str.append(str(stmt).strip())

        return "\n".join(new_stmts_str).strip()
    
    def assertCorrectRewrite(self, program: str, expected_program: str):
        program = program.strip()
        new_program = self.apply_negated_literals_transformer(program)

        if expected_program is not None:
            expected_program = expected_program.strip()
            self.assertEqual(new_program, expected_program)
    
    def test_empty(self):
        self.assertCorrectRewrite("a.","a.")
    
    def test_empty(self):
        self.assertCorrectRewrite("b, not a.","b; not a.")