import textwrap
import unittest

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.ast import RewriteContext
from fasp.ast.protecting import protect_comparisons, restore_comparisons
from fasp.ast.tree_sitter.parser import parse_string
from fasp.util.ast import AST

from fasp.ast.rewriting import (
    _functional2asp,
    normalize_ast,
    ParsingException,
    HeadAggregateToBodyRewriteTransformer,
)

# def normalize_statements(
#     library: Library, statements: Iterable[StatementAST]
# ) -> Iterable[StatementAST]:
#     """
#     Normalize a list of AST statements by rewriting them to a functional form.

#     Args:
#         statements (Iterable[StatementAST]): The AST statements to normalize.

#     Returns:
#         StatementAST: The normalized AST statements.
#     """
#     rewrite_context = RewriteContext(library)
#     return (
#         restore_comparisons(rewrite_statement(statement, rewrite_context))
#         for statement in protect_comparisons(statements)
#     )


class TestSyntacticChecker(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.lib = Library()
        self.rewrite_context = RewriteContext(self.lib)

    def assertEqualRewrite(self, program, expected):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """
        statements = parse_string(self.lib, program)

        _, result = _functional2asp(self.lib, statements)

        expected_lines = [line.strip() for line in expected.splitlines()]

        self.maxDiff = None
        self.assertCountEqual(
            list(map(lambda x: str(x).strip(), result)), expected_lines
        )
        for statement in result:
            self.assertIsInstance(statement, AST)

    def test_correct(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            { d }.
            a :- b.
            b :- not c.
            c :- c.
            f:=X :- X=1; g=h.
            f:=X :- X=1; not g=h.
            f2(X):=Y :- p(X,Y).
            f3(X):=a :- p(X).
            f4(X):=5 :- p(X).
            f3(X):=a(b,5) :- p(X).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            { d }.
            a :- b.
            b :- not c.
            c :- c.
            Ff(X) :- X=1; g=h.
            Ff(X) :- X=1; not g=h.
            Ff2(X,Y) :- p(X,Y).
            Ff3(X,a) :- p(X).
            Ff4(X,5) :- p(X).
            Ff3(X,a(b,5)) :- p(X).
            :- Ff(_); 1 > #count { V: Ff(V) }.
            :- Ff2(X0,_); 1 > #count { V: Ff2(X0,V) }.
            :- Ff3(X0,_); 1 > #count { V: Ff3(X0,V) }.
            :- Ff4(X0,_); 1 > #count { V: Ff4(X0,V) }.
        """
        ).strip()
        self.assertEqualRewrite(program, expected)

    def test_symbol_argument(self):
        """Test syntax checking with a correct program snippet."""
        program = """\
            f0 := X :- X=1; g=h.
            f1(a) := X :- X=1; not g=h.
            f1(5) := X :- X=1; not g=h.
            f2(a,b) := X :- X=1; not g=h.
        """
        expected = textwrap.dedent(
            """\
            #program base.
            Ff0(X) :- X=1; g=h.
            Ff1(a,X) :- X=1; not g=h.
            Ff1(5,X) :- X=1; not g=h.
            Ff2(a,b,X) :- X=1; not g=h.
            :- Ff0(_); 1 > #count { V: Ff0(V) }.
            :- Ff1(X0,_); 1 > #count { V: Ff1(X0,V) }.
            :- Ff2(X0,X1,_); 1 > #count { V: Ff2(X0,X1,V) }.
        """
        ).strip()
        self.assertEqualRewrite(program, expected)


# class TestNormalizeStatements(unittest.TestCase):
#     """
#     Test class for the normalization of AST statements.
#     """

#     def setUp(self):
#         """
#         Set up the test case with a library instance.
#         """
#         self.lib = Library()

#     def assertEqualNormalization(self, program, expected=None):
#         statements = []

#         def callback(statement):
#             statements.append(statement)

#         ast.parse_string(self.lib, program, callback)
#         program_l = [str(stmt).strip() for stmt in statements]

#         result = restore_comparisons(
#             self.lib, protect_comparisons(self.lib, statements)
#         )
#         result = [str(stmt).strip() for stmt in result]
#         self.maxDiff = None
#         if expected is None:
#             self.assertCountEqual(result, program_l)

#         result = normalize_ast(self.lib, statements)
#         result = [str(stmt).strip() for stmt in result]
#         if expected is None:
#             expected = program_l
#         else:
#             expected = [line.strip() for line in expected.splitlines()]
#         self.assertCountEqual(result, expected)

#     def test_normalize_statements_basic(self):
#         """Test normalization of AST statements."""
#         program = """
#             a :- b.
#             b :- not c.
#             c :- c.
#             f = X :- X=1; g=h.
#             f=X :- X=1; not g=h.
#             f2(X) = Y :- p(X,Y).
#             f3(X) = a :- p(X).
#             f4(X) = 5 :- p(X).
#             f3(X) = a(b,5) :- p(X).
#         """
#         self.assertEqualNormalization(program)

#     def test_normalize_statements_choices_aggregates(self):
#         """Test normalization of AST statements."""
#         program = """
#             { d }.
#             #sum { X: p(X,Y): q(X) } :- r(Y).
#             b :- not c.
#             c :- c.
#             f=X :- X=1; g=h.
#             f=X :- X=1; not g=h.
#             f2(X)=Y :- p(X,Y).
#             f3(X)=a :- p(X).
#             f4(X)=5 :- p(X).
#             f3(X)=a(b,5) :- p(X).
#         """
#         expected = textwrap.dedent(
#             """\
#             #program base.
#             #count { 0,d: d }.
#             #sum { X: p(X,Y): q(X) } :- r(Y).
#             b :- not c.
#             c :- c.
#             f=X :- X=1; g=h.
#             f=X :- X=1; not g=h.
#             f2(X)=Y :- p(X,Y).
#             f3(X)=a :- p(X).
#             f4(X)=5 :- p(X).
#             f3(X)=a(b,5) :- p(X).
#         """
#         ).strip()
#         self.assertEqualNormalization(program, expected)

#     def test_functional2asp_head_aggregate_errors(self):
#         """Test that head aggregates trigger a ParsingException when invalid."""

#         # Program with an invalid head aggregate (on the right)
#         program = "f(Y) = #sum{ X : p(X) } = f(Y) :- q(Y)."

#         statements = []
#         ast.parse_string(self.lib, program, statements.append)

#         # _functional2asp should raise ParsingException
#         with self.assertRaises(ParsingException) as cm:
#             _functional2asp(self.lib, statements)

#         exc = cm.exception
#         # Check error with a message about right-hand aggregate
#         self.assertEqual(len(exc.errors), 1)
#         self.assertEqual(
#             "Wrong assignment syntax: f(Y) = #sum { X: p(X) } = f(Y)",
#             exc.errors[0].message,
#         )
