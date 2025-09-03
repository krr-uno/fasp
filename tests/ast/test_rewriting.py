import textwrap
import unittest

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.ast import RewriteContext
from fasp.ast.protecting import protect_comparisons, restore_comparisons
from fasp.util.ast import AST

from fasp.ast.rewriting import _functional2asp, normalize_ast, ParsingException, HeadAggregateToBodyRewriteTransformer

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
        statements = []

        def callback(statement):
            statements.append(statement)

        ast.parse_string(self.lib, program, callback)

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
            f = X :- X=1; g=h.
            f=X :- X=1; not g=h.
            f2(X) = Y :- p(X,Y).
            f3(X) = a :- p(X).
            f4(X) = 5 :- p(X).
            f3(X) = a(b,5) :- p(X).
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
            f0 = X :- X=1; g=h.
            f1(a) = X :- X=1; not g=h.
            f1(5) = X :- X=1; not g=h.
            f2(a,b) = X :- X=1; not g=h.
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


class TestNormalizeStatements(unittest.TestCase):
    """
    Test class for the normalization of AST statements.
    """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.lib = Library()

    def assertEqualNormalization(self, program, expected=None):
        statements = []

        def callback(statement):
            statements.append(statement)

        ast.parse_string(self.lib, program, callback)
        program_l = [str(stmt).strip() for stmt in statements]

        result = restore_comparisons(
            self.lib, protect_comparisons(self.lib, statements)
        )
        result = [str(stmt).strip() for stmt in result]
        self.maxDiff = None
        if expected is None:
            self.assertCountEqual(result, program_l)

        result = normalize_ast(self.lib, statements)
        result = [str(stmt).strip() for stmt in result]
        if expected is None:
            expected = program_l
        else:
            expected = [line.strip() for line in expected.splitlines()]
        self.assertCountEqual(result, expected)

    def test_normalize_statements_basic(self):
        """Test normalization of AST statements."""
        program = """
            a :- b.
            b :- not c.
            c :- c.
            f = X :- X=1; g=h.
            f=X :- X=1; not g=h.
            f2(X) = Y :- p(X,Y).
            f3(X) = a :- p(X).
            f4(X) = 5 :- p(X).
            f3(X) = a(b,5) :- p(X).
        """
        self.assertEqualNormalization(program)

    def test_normalize_statements_choices_aggregates(self):
        """Test normalization of AST statements."""
        program = """
            { d }.
            #sum { X: p(X,Y): q(X) } :- r(Y).
            b :- not c.
            c :- c.
            f=X :- X=1; g=h.
            f=X :- X=1; not g=h.
            f2(X)=Y :- p(X,Y).
            f3(X)=a :- p(X).
            f4(X)=5 :- p(X).
            f3(X)=a(b,5) :- p(X).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            #count { 0,d: d }.
            #sum { X: p(X,Y): q(X) } :- r(Y).
            b :- not c.
            c :- c.
            f=X :- X=1; g=h.
            f=X :- X=1; not g=h.
            f2(X)=Y :- p(X,Y).
            f3(X)=a :- p(X).
            f4(X)=5 :- p(X).
            f3(X)=a(b,5) :- p(X).
        """
        ).strip()
        self.assertEqualNormalization(program, expected)
    
    def test_functional2asp_head_aggregate_errors(self):
        """Test that head aggregates trigger a ParsingException when invalid."""

        # Program with an invalid head aggregate (on the right)
        program = "#sum{ X : p(X) } = f(Y) :- q(Y)."

        statements = []
        ast.parse_string(self.lib, program, statements.append)

        # _functional2asp should raise ParsingException
        with self.assertRaises(ParsingException) as cm:
            _functional2asp(self.lib, statements)

        exc = cm.exception
        # Check error with a message about right-hand aggregate
        self.assertEqual(len(exc.errors), 1)
        self.assertIn(
            "Head aggregate cannot appear on the right-hand side", exc.errors[0].message
        )

class TestHeadAggregateToBodyRewriteTransformer(unittest.TestCase):
    """
    Unit tests for the HeadAggregateToBodyRewriteTransformer.
    """

    def setUp(self):
        self.lib = Library()

    def parse_program(self, program: str):
        stmts = []
        ast.parse_string(self.lib, program, stmts.append)
        return stmts

    def rewrite(self, program: str):
        stmts = self.parse_program(program)
        rewriter = HeadAggregateToBodyRewriteTransformer(self.lib)
        out = rewriter.rewrite_statements(stmts)
        return [str(stmt).strip() for stmt in out], rewriter.errors

    def assertRewriteEqual(self, program: str, expected: str):
        result, errors = self.rewrite(program)
        expected_lines = [line.strip() for line in expected.splitlines()]
        self.assertCountEqual(result, expected_lines)
        # also check that no errors were collected
        self.assertEqual(errors, [])

    def test_valid_sum_and_count(self):
        program = """\
            f(X) = #sum{ Y : p(Y,Z) : q(X), r(X) } :- b(X,Z).
            f(X) = #count{ Y : p(Y,Z) } :- b(X,Z).
        """
        expected = textwrap.dedent("""\
            #program base.
            f(X)=W :- b(X,Z); W = #sum { Y: p(Y,Z), q(X), r(X) }.
            f(X)=W :- b(X,Z); W = #count { Y: p(Y,Z) }.
        """).strip()
        self.assertRewriteEqual(program, expected)

    def test_aggregate_on_right_side_error(self):
        program = """\
            #sum{ Y : p(Y,Z) } = f(X) :- b(X,Z).
        """
        _, errors = self.rewrite(program)
        self.assertEqual(len(errors), 1)
        self.assertIn("Head aggregate cannot appear on the right-hand side", errors[0].message)
        # change to: Head aggregate cannot appear on the right-hand side of the assignment

    def test_non_equality_relation_error(self):
        program = """\
            f(X) < #sum{ Y : p(Y,Z) } :- b(X,Z).
        """
        _, errors = self.rewrite(program)
        self.assertEqual(len(errors), 1)
        self.assertIn("must use '='", errors[0].message)
        # change to: aggregates with comparisons cannot not be used in the head, found "f(X) < #sum{ Y : p(Y,Z) }", assignments are of the form "f(X) = #sum{ Y : p(Y,Z) }".

    def test_invalid_left_term_error(self):
        program = """\
            a = #sum{ Y : p(Y,Z) } :- b(X,Z).
        """
        _, errors = self.rewrite(program)

        self.assertEqual(len(errors), 1)
        self.assertIn("must be a function term", errors[0].message)
        # This is correct
        # a is TermSymbolic whose symbol is a function. Numbers or string are not valid here.

    def test_invalid_left_term_error_number(self):
        program = """\
            1 = #sum{ Y : p(Y,Z) } :- b(X,Z).
        """
        _, errors = self.rewrite(program)

        self.assertEqual(len(errors), 1)
        self.assertIn("must be a function term", errors[0].message)
        # Change to: The right-hand side of an assignment must be a function term

    def test_non_rule_statement_passthrough(self):
        program = "p(a)."

        stmts = self.parse_program(program)
        rewriter = HeadAggregateToBodyRewriteTransformer(self.lib)
        out = rewriter.rewrite_statements(stmts)

        self.assertEqual("\n".join(str(s) for s in out).strip(),
                 "#program base.\np(a).")        
        self.assertEqual(rewriter.errors, [])

    def test_error_method_records(self):
        rewriter = HeadAggregateToBodyRewriteTransformer(self.lib)

        # Construct Positions for dummy AST
        begin = Position(self.lib, "test.lp", 1, 1)
        end = Position(self.lib, "test.lp", 1, 5)
        dummy_loc = Location(begin, end)

        rewriter._error(dummy_loc, "dummy message", ast.HeadAggregate)

        self.assertEqual(len(rewriter.errors), 1)
        self.assertEqual(rewriter.errors[0].message, "dummy message")
        self.assertIs(rewriter.errors[0].information, ast.HeadAggregate)
        # change to program "f(X) = #sum{ Y : p(Y,Z) : q(Y,Z) } :- b(X,Z)."
    
    def test_malformed_head_aggregate(self):
        # create a dummy HeadAggregate with left=None
        lib = self.lib
        begin = Position(self.lib, "test.lp", 1, 1)
        end = Position(self.lib, "test.lp", 1, 5)
        dummy_loc = Location(begin, end)

        element = ast.HeadAggregateElement(
            lib,
            dummy_loc,
            tuple=[ast.TermVariable(lib, dummy_loc, "X")],
            literal=ast.LiteralBoolean(lib, dummy_loc, ast.Sign.NoSign, True),
            condition=[]
        )

        head_agg = ast.HeadAggregate(
            lib,
            dummy_loc,
            None,  # left guard missing
            ast.AggregateFunction.Sum,
            [element],
            None,  # right guard
        )

        stmt = ast.StatementRule(lib, dummy_loc, head_agg, [])
        rewriter = HeadAggregateToBodyRewriteTransformer(lib)
        out = rewriter._rewrite_statement(stmt)
        
        # The original statement is returned unchanged
        self.assertIs(out, stmt)
        
        # An error should be recorded
        self.assertEqual(len(rewriter.errors), 1)
        self.assertIn("missing left guard", rewriter.errors[0].message)
        # try with a string again
    
    def test_valid_max_min(self):
        program = """\
        f(X) = #max{ Y : p(Y) } :- b(X).
        f(X) = #min{ Y : q(Y) } :- b(X).
        """
        expected = textwrap.dedent("""\
            #program base.
            f(X)=W :- b(X); W = #max { Y: p(Y) }.
            f(X)=W :- b(X); W = #min { Y: q(Y) }.
        """).strip()
        self.assertRewriteEqual(program, expected)

    def test_used_variables(self):
        program = """\
        f(W) = #max{ W2 : p(W2) } :- b(W).
        """
        expected = textwrap.dedent("""\
            #program base.
            f(W)=W3 :- b(W); W3 = #max { W2: p(W2) }.
        """).strip()
        self.assertRewriteEqual(program, expected)