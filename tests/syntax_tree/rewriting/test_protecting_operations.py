import unittest

from clingo import ast

from funasp.util.ast import AST,ELibrary

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree.rewritings.protecting_operations import protect_operations, restore_operations
from funasp.syntax_tree.rewritings.integration import RewritingStatement, _clingo_rewrite

class TestProtectOperations(unittest.TestCase):
    def setUp(self):
        self.lib = ELibrary()
        self.context = RewriteContext(lib=self.lib)

    def assertEqualRewrite(self, program, expected):
        statements = []

        def callback(statement):
            statements.append(statement)

        ast.parse_string(self.lib.library, program, callback)

        result = [protect_operations(self.context, stmt) for stmt in statements]

        expected_lines = [line.strip() for line in expected.splitlines()]
        expected_lines = [line for line in expected_lines if line]  # Remove empty lines
        result = result[1:]
        self.maxDiff = None
        self.assertCountEqual(
            list(map(lambda x: str(x).strip(), result)), expected_lines
        )
        for statement in result:
            self.assertIsInstance(statement, AST)



    ## TESTS ##
    def test_protect_operations(self):
        self.assertEqualRewrite(
            """
            p(a+((b+d),c),X) :-  f(X).
            p(|(a+b,c)|).
            """,
            """
            p(OP(6,a,(OP(6,b,d),c)),X) :- f(X).
            p(OP(9,(OP(6,a,b),c))).
            """
        )

    ## EXTRA TEST ##
    def test_protect_operations_2(self):
        self.assertEqualRewrite(
            """
            p(a+((b+d),c),X) :-  f(X) + f(Y)=2.
            """,
            """
            p(OP(6,a,(OP(6,b,d),c)),X) :- OP(6,f(X),f(Y))=2.
            """
        )



class TestRestoreOperations(unittest.TestCase):
    def setUp(self):
        self.lib = ELibrary()
        self.context = RewriteContext(lib=self.lib)

    def assertEqualRewrite(self, program, expected):
        statements = []

        def callback(statement):
            statements.append(statement)

        ast.parse_string(self.lib.library, program, callback)
        result = [protect_operations(self.context, stmt) for stmt in statements]
        result2 = []
        for stmt in result:
            stmt = RewritingStatement(stmt)
            stmt.rewrite_to_clingo(self.context, lambda c, s: s)
            _clingo_rewrite(self.context, stmt)
            result2.extend(stmt._clingo_rewritten)
        result = [restore_operations(self.context, stmt) for stmt in result2]

        expected_lines = [line.strip() for line in expected.splitlines()]
        expected_lines = [line for line in expected_lines if line]  # Remove empty lines
        result = result[1:]
        self.maxDiff = None
        self.assertCountEqual(
            list(map(lambda x: str(x).strip(), result)), expected_lines
        )
        for statement in result:
            self.assertIsInstance(statement, AST)



    ## TESTS ##
    def test_restore_operations(self):
        program = "p(a+(b+d,c),X) :- f(X)."
        self.assertEqualRewrite(
            program, program
        )

    # Non-deterministic results
    # def test_restore_operations_head_disjunction(self):
    #     program = """
    #         p(a+(b+d,c),X); g(a+b) :- f(X).
    #         """
    #     self.assertEqualRewrite(
    #         program, program
    #     )

    def test_restore_operations_absolute(self):
        program ="p(|(a+b,c)|)."
        self.assertEqualRewrite(program, program)

    def test_restore_mixed_non_function(self):
        program = "p(1,a,2)."
        self.assertEqualRewrite(program, program)
    ## EXTRA TEST ##
    def test_restore_nested_functions(self):
        program = "p(f(g(a+b),h(c)))."
        self.assertEqualRewrite(program, program)
