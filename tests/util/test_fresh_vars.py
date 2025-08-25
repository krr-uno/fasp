import unittest
from clingo import ast
from clingo.core import Library, Position, Location
from fasp.util import ast as util_ast


class TestFreshVariableManager(unittest.TestCase):
    """Tests FreshVariableManager for creating fresh variables."""

    def setUp(self):
        self.lib = Library()
        self.loc = Location(Position(self.lib, "<stdin>", 1, 1),
                            Position(self.lib, "<stdin>", 1, 1))
        util_ast.FreshVariableManager._used.clear()
        util_ast.FreshVariableManager._initialized = False

    def parse_program(self, program: str):
        stmts = []
        ast.parse_string(self.lib, program, stmts.append)
        return stmts

    def test_fresh_variable_basic(self):
        program = """
            f(X,Y) = Z :- p(X,Y), q(Z).
            g(A,B) :- r(A), s(B).
        """
        stmts = self.parse_program(program)
        util_ast.FreshVariableManager.collect_vars(stmts)

        # First call collects all variables
        v1 = util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="X")
        v2 = util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="Y")
        v3 = util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="W")

        self.assertEqual(v1.name, "X1")  # X exists → start numbering at 1
        self.assertEqual(v2.name, "Y1")  # Y exists → start numbering at 1
        self.assertEqual(v3.name, "W")   # W not used → no number

        
    def test_fresh_variable_numbered_base(self):
        program = """
            a(V1,V2) :- b(V1), c(V2).
        """
        stmts = self.parse_program(program)
        util_ast.FreshVariableManager.collect_vars(stmts)
        v1 = util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="V")
        v2 = util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="V")
        v3 = util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="V")

        self.assertEqual(v1.name, "V")  # V does not exist → so return V
        self.assertEqual(v2.name, "V3")   # V, V1 and V2 are all used → so return V3
        self.assertEqual(v3.name, "V4")  # V4 is returned since V, V1, V2 and V3 are all used

    def test_fresh_variable_monotonic_sequence(self):
        program = """
            p(X,Y) :- q(X), r(Y).
            s(Z) :- t(Z).
        """
        stmts = self.parse_program(program)
        util_ast.FreshVariableManager.collect_vars(stmts)
        names = [util_ast.FreshVariableManager.fresh_variable(self.lib, self.loc, base="X").name for _ in range(3)]
        self.assertEqual(names, ["X1", "X2", "X3"])

        self.assertEqual(names, ["X1", "X2", "X3"])
        # Check all names are in used set
        for name in names + ["X", "Y", "Z"]:
            self.assertIn(name, util_ast.FreshVariableManager._used)



if __name__ == "__main__":
    unittest.main()
