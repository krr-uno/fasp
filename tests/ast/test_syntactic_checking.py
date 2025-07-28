import unittest

from clingo import ast
from clingo.ast import ASTType

from clingox.ast import normalize_symbolic_terms

from fasp.util.ast import SyntacticCheckVisitor, SyntacticError

INVALID_ASTTYPES = {
    ASTType.Aggregate,
    ASTType.ConditionalLiteral,
    ASTType.Defined,
    ASTType.Disjunction,
    ASTType.Edge,
    ASTType.HeadAggregate,
    ASTType.HeadAggregateElement,
    ASTType.Heuristic,
    ASTType.Id,
    ASTType.Minimize,
    ASTType.ProjectAtom,
    ASTType.ProjectSignature,
    ASTType.Script,
    ASTType.TheoryAtom,
    ASTType.TheoryAtomDefinition,
    ASTType.TheoryAtomElement,
    ASTType.TheoryDefinition,
    ASTType.TheoryFunction,
    ASTType.TheoryGuard,
    ASTType.TheoryGuardDefinition,
    ASTType.TheoryOperatorDefinition,
    ASTType.TheorySequence,
    ASTType.TheoryTermDefinition,
    ASTType.TheoryUnparsedTerm,
    ASTType.TheoryUnparsedTermElement,
}


class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def assertEqualErrors(self, program, expected_errors):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """

        syntactic_checker = SyntacticCheckVisitor(INVALID_ASTTYPES)

        def callback(statement):
            statement = normalize_symbolic_terms(statement)
            statement = syntactic_checker.visit(statement)

        ast.parse_string(program, callback)
        self.assertCountEqual(syntactic_checker.errors, expected_errors)

    def test_correct(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            #program actions.
            a :- b.
            b :- not c.
            c :- c.
            f = 1 :- g = h.
            f = 1 :- not g = h.
        """
        self.assertEqualErrors(program, [])

    def test_incorrect(self):
        """Test syntax checking with an incorrect program snippet."""
        program = """
            #program actions.
            a :- b : c.
            a, c :- b.
            {b} :- not c.
            c :- c.
            f = 1 :- g = h.
            f = 1 :- not g = h.
        """
        expected_errors = [
            SyntacticError(
                ast.Location(
                    ast.Position("<string>", 3, 18), ast.Position("<string>", 3, 23)
                ),
                "unexpected b: c",
                ASTType.ConditionalLiteral,
            ),
            SyntacticError(
                ast.Location(
                    ast.Position("<string>", 4, 13), ast.Position("<string>", 4, 17)
                ),
                "unexpected a; c",
                ASTType.Disjunction,
            ),
            SyntacticError(
                ast.Location(
                    ast.Position("<string>", 5, 13), ast.Position("<string>", 5, 16)
                ),
                "unexpected { b }",
                ASTType.Aggregate,
            ),
        ]
        self.assertEqualErrors(program, expected_errors)
