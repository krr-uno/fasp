import textwrap
import unittest

from clingo import ast
from clingo.core import Library

from funasp.syntax_tree._nodes import FASP_AST
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.syntax_tree._context import RewriteContext

from funasp.syntax_tree.rewritings.protecting import (
    _ComparisonRestorationTransformer,
    _protect_comparison,
    _restore_guard_arguments,
    protect_comparisons,
    restore_comparison,
    restore_comparisons_list,
    protect_assignments,
    restore_assignments_list,
)
from funasp.util.ast import AST, ELibrary


def _restore_guard(library: Library, term: ast.TermFunction) -> ast.RightGuard:
    right = _restore_guard_arguments(library, term)
    return ast.RightGuard(library, right.relation, right.term)


class TestProtectComparisons(unittest.TestCase):
    """ """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.elib = ELibrary()
        self.context = RewriteContext(self.elib)
        self.lib = self.elib.library
        self.rewrite_context = ast.RewriteContext(self.lib)

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

        result = [protect_comparisons(self.context, stmt) for stmt in statements]

        expected_lines = [line.strip() for line in expected.splitlines()]

        self.maxDiff = None
        self.assertCountEqual(
            list(map(lambda x: str(x).strip(), result)), expected_lines
        )
        for statement in result:
            self.assertIsInstance(statement, AST)

    def test_basic(self):
        """Test syntax checking with a correct program snippet."""
        program = """\
            f = 100 :- not g = 100.
            f(X) > 100 :- 1000 < g(X,Y) < 1010.
            f(a,5) >= 100 :- g("string") <= 100, not h(3+5) != 300, not not z = p.
        """
        expected = textwrap.dedent(
            """\
            #program base.
            CMP(f,(GRD(0,100),),0) :- CMP(g,(GRD(0,100),),1).
            CMP(f(X),(GRD(1,100),),0) :- CMP(1000,(GRD(3,g(X,Y)),GRD(3,1010)),0).
            CMP(f(a,5),(GRD(2,100),),0) :- CMP(g("string"),(GRD(4,100),),0); CMP(h(3+5),(GRD(5,300),),1); CMP(z,(GRD(0,p),),2).
        """
        ).strip()
        self.assertEqualRewrite(program, expected)

    def test_aggregates(self):
        """Test syntax checking with a correct program snippet."""
        program = """\
            {f = 100} = 1 :- #count{ X,Y : g = X, not h = Y } > 5.
            #count { X : f(X) = Y : p(X) } :- q(Y).
            #sum { X : f(X) = Y : p(X) } :- q(Y).
        """
        expected = textwrap.dedent(
            """\
            #program base.
            { CMP(f,(GRD(0,100),),0) } = 1 :- #count { X,Y: CMP(g,(GRD(0,X),),0), CMP(h,(GRD(0,Y),),1) } > 5.
            #count { X: CMP(f(X),(GRD(0,Y),),0): p(X) } :- q(Y).
            #sum { X: CMP(f(X),(GRD(0,Y),),0): p(X) } :- q(Y).
        """
        ).strip()
        self.assertEqualRewrite(program, expected)


class TestRestoreComparisons(unittest.TestCase):
    """ """

    def setUp(self):
        """
        Set up the test case with a library instance.
        """
        self.elib = ELibrary()
        self.lib = self.elib.library
        self.context = RewriteContext(self.elib)
        self.rewrite_context = ast.RewriteContext(self.lib)


    def test_restore(self):
        cmp = ast.parse_literal(self.lib, "a=100")
        self.assertEqual(str(cmp), "a=100")
        # transformer = _ComparisonProtectorTransformer(self.context)
        # protected = transformer.dispatch(cmp)
        protected = _protect_comparison(cmp, self.context) or cmp
        self.assertEqual(str(protected), "CMP(a,(GRD(0,100),),0)")
        guard = protected.atom.pool[0].arguments[1].pool[0].arguments[0]
        self.assertEqual(str(guard), "GRD(0,100)")
        self.assertIsInstance(guard, ast.TermFunction)
        restored_guard = _restore_guard(self.lib, guard)
        self.assertEqual(str(restored_guard), " = 100")
        restored = restore_comparison(self.lib, protected)
        self.assertEqual(str(restored), "a=100")
        self.assertIsInstance(restored, ast.LiteralComparison)

    def test_restore_symbolic(self):
        lit = ast.parse_literal(self.lib, "#true")
        # transformer = _ComparisonProtectorTransformer(self.context)
        # protected = transformer.dispatch(lit)
        protected = _protect_comparison(lit, self.context) or lit
        restorer = _ComparisonRestorationTransformer(self.context)
        restored = restorer.dispatch(protected)
        self.assertEqual(lit, restored)

    def assertEqualRestore(self, program: str, expected: str):
        """
        Parses `program`, protects comparisons, restores them, and
        checks the restored program matches the original AST structure.
        """
        statements = []

        def callback(statement):
            statements.append(statement)

        ast.parse_string(self.lib, program, callback)

        # Protect comparisons
        protected = [protect_comparisons(self.context, stmt) for stmt in statements]

        protected_str = "\n".join(str(stmt).strip() for stmt in protected[1:])
        exp_protected_str = textwrap.dedent(expected).strip()

        self.assertEqual(
            protected_str,
            exp_protected_str,
            msg=f"Protected form mismatch.\nExpected:\n{exp_protected_str}\nGot:\n{protected_str}",
        )

        # Restore comparisons
        restored = list(restore_comparisons_list(self.context, protected))
        self.assertEqual(len(statements), len(restored))
        for orig, rest in zip(statements, restored):
            # Compare string forms
            self.assertEqual(str(orig).strip(), str(rest).strip())
            # Ensure restored node is a valid FASP AST
            self.assertIsInstance(rest, FASP_AST)

    def test_basic_comparison_restore(self):
        self.assertEqualRestore(
            "a=100.",
            "CMP(a,(GRD(0,100),),0).",
        )


class TestProtectAssignments(unittest.TestCase):
    """
    Unit tests for assignment-protection transformation.
    Mirrors TestProtectComparisons in style and structure.
    """

    def setUp(self):
        self.lib = ELibrary()
        self.context = RewriteContext(self.lib)

    def assertEqualRewrite(self, program: str, expected: str):
        """
        Parses `program`, runs protect_assignments(), and compares to `expected`.
        """

        statements = parse_string(self.lib, program)

        rewritten = list(protect_assignments(self.context, statements))[
            1:
        ]  # :1 to remove #program base.

        expected_lines = [line.strip() for line in expected.splitlines()]

        self.maxDiff = None
        self.assertCountEqual(
            list(map(lambda s: str(s).strip(), rewritten)), expected_lines
        )

        for stmt in rewritten:
            self.assertIsInstance(stmt, FASP_AST)

    def test_basic_assignments(self):
        """
        Simple rules with assignments inside heads & bodies.
        """
        program = """\
            f(X) := Y :- g.
            a := b :- p(X, Y).
            h(3) := 20.
        """

        expected = textwrap.dedent(
            """\
            ASS(f(X),Y) :- g.
            ASS(a,b) :- p(X,Y).
            ASS(h(3),20).
        """
        ).strip()

        self.assertEqualRewrite(program, expected)

    def test_aggregate(self):
        self.assertEqualRewrite(
            "{ f(X) := (Y,Z) } :- p.",
            "{ ASS(f(X),(Y,Z)) } :- p.",
        )

    def test_pool(self):
        self.assertEqualRewrite(
            "f(1;2) := Y :- g(Y).",
            "ASS(f(1;2),Y) :- g(Y).",
        )

    def test_aggregate_condition(self):
        self.assertEqualRewrite(
            "{ a := X: p(X); a } = 1 :- #count { X: p(X) } >= 1; s.",
            "{ ASS(a,X): p(X); a } = 1 :- #count { X: p(X) } >= 1; s.",
        )

    def test_choice_some_aggregate(self):
        with self.assertRaises(AssertionError) as cm:
            self.assertEqualRewrite(
                "a := #some{X: p(X)} :- p.",
                "{ ASS(f(X),(ARG(0,Y),)) } :- p.",
            )
        self.assertEqual(
            str(cm.exception),
            "ChoiceSomeAssignment seen during assignment protection. Unhandled.",
        )

    def test_head_aggregate_assignment(self):
        self.assertEqualRewrite(
            "#count { 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
            "#count { 0,ass(king(f(C)),X): ASS(king(g(C)),h(X)): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
        )


class TestRestoreAssignments(unittest.TestCase):
    """
    Unit tests for assignment restoration.
    """

    def setUp(self):
        self.lib = ELibrary()
        self.context = RewriteContext(self.lib)

    def assertEqualRestore(self, program: str, expected: str):
        """
        Parses `program`, protects assignments, restores them, and
        checks the restored program matches the original AST structure.
        """
        statements = parse_string(self.lib, program)

        # Protect assignments
        protected = list(protect_assignments(self.context, statements))

        protected_str = "\n".join(str(stmt).strip() for stmt in protected[1:])
        exp_protected_str = textwrap.dedent(expected).strip()

        self.assertEqual(
            protected_str,
            exp_protected_str,
            msg=f"Protected form mismatch.\nExpected:\n{exp_protected_str}\nGot:\n{protected_str}",
        )

        # Restore assignments
        restored = list(restore_assignments_list(self.lib, protected))
        self.assertEqual(len(statements), len(restored))
        for orig, rest in zip(statements, restored):
            # Compare string forms
            self.assertEqual(str(orig).strip(), str(rest).strip())
            # Ensure restored node is a valid FASP AST
            self.assertIsInstance(rest, FASP_AST)

    def test_basic_assignments(self):
        program = """\
            f(X) := Y :- g.
            a := b :- p(X, Y).
            h(3) := 20.
        """
        expected = """\
            ASS(f(X),Y) :- g.
            ASS(a,b) :- p(X,Y).
            ASS(h(3),20).
        """
        self.assertEqualRestore(program, expected)

    def test_aggregate_assignments(self):
        self.assertEqualRestore(
            "{ f(X) := (Y,Z) } :- p.",
            "{ ASS(f(X),(Y,Z)) } :- p.",
        )

    def test_choice_assignments(self):
        self.assertEqualRestore(
            "f(1;2) := Y :- g(Y).",
            "ASS(f(1;2),Y) :- g(Y).",
        )

    def test_restore_non_function_atom(self):
        self.assertEqualRestore(
            "#true.",
            "#true.",
        )

    def test_no_assignment(self):
        self.assertEqualRestore(
            "f(X) :- g(Y).",
            "f(X) :- g(Y).",
        )

    def test_no_assignment_in_aggregate(self):
        self.assertEqualRestore(
            "{ f(X) } :- g(Y).",
            "{ f(X) } :- g(Y).",
        )

    def test_head_aggregate_Assignment(self):
        self.assertEqualRestore(
            "#count { 0,ass(king(C),X): king(C) := X: person(X) } :- country(C).",
            "#count { 0,ass(king(C),X): ASS(king(C),X): person(X) } :- country(C).",
        )
