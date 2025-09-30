import unittest


from fasp.ast.collectors import collect_evaluable_functions, collect_variables
from fasp.ast.parsing.parser import parse_string
from fasp.util.ast import ELibrary


class TestGetEvaluableFunctions(unittest.TestCase):
    """
    Test class for the syntactic checker.
    """

    def setUp(self):
        self.lib = ELibrary()

    def assertEqualFunctions(self, program, expected_functions):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """
        statements = parse_string(self.lib, program)
        evaluable_functions = collect_evaluable_functions(statements)
        self.assertCountEqual(
            set(map(str, evaluable_functions)), set(expected_functions)
        )

    def test_get_evaluable_functions(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            #program base.
            a :- b.
            b :- not c.
            c :- c.
            f := 1 :- g = h.
            f := 1 :- not g = h.
            f2(X) := 1 :- p(X).
        """
        self.assertEqualFunctions(program, ["f/0", "f2/1"])

    def test_fib(self):
        """Test syntax checking with a correct program snippet."""
        program = """
            #program base.
            fibo(X) := F1+F2 :- number(X),X>1, fibo(X-1) = F1, fibo(X-2) = F2.
        """
        self.assertEqualFunctions(program, ["fibo/1"])


class TestGetVariables(unittest.TestCase):

    def setUp(self):
        self.lib = ELibrary()

    def assertEqualVariables(self, program, expected_variables):
        """
        Helper method to assert that the syntactic checker finds the expected errors.

        Args:
            program (str): The program to check.
            expected_errors (list): The list of expected SyntacticError instances.
        """
        statements = parse_string(self.lib, program)
        variables = collect_variables(statements[1])
        self.assertCountEqual(variables, expected_variables)

    def test_collect_from_clingo_program(self):
        self.assertEqualVariables("p(X,Y) :- q(Z), not r(W).", {"X", "Y", "Z", "W"})
        self.assertEqualVariables("p(X,Y).", {"X", "Y"})
        self.assertEqualVariables("p(X,Y) := (W,Z).", {"X", "Y", "W", "Z"})
        self.assertEqualVariables("a := #sum{ X,Z : p(X,Y)}.", {"X", "Y", "Z"})
        self.assertEqualVariables(
            "r(A) :- #count { B : s(B,C) } > D, t(E,F).", {"A", "B", "C", "D", "E", "F"}
        )
        self.assertEqualVariables(
            "u(G) :- v(H), v(I), w(J), w(K), L = M+N, O != P.",
            {"G", "H", "I", "J", "K", "L", "M", "N", "O", "P"},
        )

    # def test_incorrect_1(self):
    #     """Test syntax checking with an incorrect program snippet."""
    #     program = """\
    #         a :- b.
    #         b :- not c.
    #         c :- c.
    #         f > 1 :- g = h.
    #         f := 1 :- not g = h.
    #         f2(X) := 1 :- p(X).
    #         5 := f :- q(Y).
    #     """
    #     with self.assertRaises(ParsingException) as captured_stderr:
    #         self.assertEqualFunctions(program, ["f/0", "f2/1"])
    #     messages = map(
    #         lambda e: (e.location.begin.line, e.message),
    #         captured_stderr.exception.errors,
    #     )
    #     self.assertCountEqual(
    #         messages,
    #         [
    #             (
    #                 4,
    #                 "unexpected comparison f>1 in the head. Assignments are of the form 'FUNCTION = TERM'.",
    #             ),
    #             (
    #                 7,
    #                 "unexpected comparison 5=f in the head. Assignments are of the form 'FUNCTION = TERM'.",
    #             ),
    #         ],
    #     )

    # def test_incorrect_2(self):
    #     """Test syntax checking with an incorrect program snippet."""
    #     program = """\
    #         a :- b.
    #         b :- not c.
    #         c :- c.
    #         f := 1 :- g = h.
    #         not f = 1 :- not g = h.
    #         f2(X) := 1 :- p(X).
    #         f := 5  :- q(Y).
    #     """
    #     with self.assertRaises(ParsingException) as captured_stderr:
    #         self.assertEqualFunctions(program, ["f/0", "f2/1"])
    #     messages = map(
    #         lambda e: (e.location.begin.line, e.message),
    #         captured_stderr.exception.errors,
    #     )
    #     self.assertCountEqual(
    #         messages,
    #         [
    #             (
    #                 5,
    #                 "unexpected negated comparison not f=1 in the head. Assignments are of the form 'FUNCTION = TERM'.",
    #             ),
    #         ],
    #     )

    # def test_choice(self):
    #     """Test syntax checking with a correct program snippet."""
    #     program = """
    #         { a = 1 } :- b.
    #         { f(X) = Y : p(Y) } :- q(X).
    #         { f(X) = Y : g = Y } :- q(X).
    #     """
    # self.assertEqualFunctions(program, ["a/0", "f/1"])
