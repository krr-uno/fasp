import textwrap
from typing import Any, Iterable, Sequence
import unittest

from attr import has
from clingo import ast
from clingo.core import Location, Position, Library

from fasp.ast import AssignmentRule
from fasp.util.ast import AST, SyntacticCheckVisitor, SyntacticError, TermAST
from fasp.parsing import parser
from tests.parsing.tree_sitter_util import format_ts_tree


class TestParseAssignment(unittest.TestCase):

    def _dispatch(self, ast1: Any, ast2: Any, whole1: AST, whole2: AST, parents1: list[AST] | None = None, parents2: list[AST] | None = None) -> None:
        if not isinstance(ast1, AST) or not isinstance(ast2, AST):
            return
        if parents1 is None:
            parents1 = []
        if parents2 is None:
            parents2 = []
        parents1.append((type(ast1), ast1))
        parents2.append((type(ast2), ast2))
        self.assertEqual(
            type(ast1), type(ast2), 
            textwrap.dedent(f"""\
            Type mismatch in {str(whole1)} vs {str(whole2)}
            '{str(ast1)}' vs '{str(ast2)}'
            Parents1: {' > '.join(f'{t.__name__}[{str(a)}]' for t, a in parents1)}
            Parents2: {' > '.join(f'{t.__name__}[{str(a)}]' for t, a in parents2)}
            """
            )
        )
        children = [m for m in dir(ast1) if not m.startswith("_")]
        for child in children:
            attr1 = getattr(ast1, child)
            attr2 = getattr(ast2, child)
            if callable(attr1) or callable(attr2):
                continue
            if isinstance(attr1, Iterable):
                for a1, a2 in zip(attr1, attr2):
                    self._dispatch(a1, a2, whole1, whole2, parents1, parents2)
            else:
                # print(f"Dispatching {child}: {attr1} vs {attr2}")
                self._dispatch(attr1, attr2, whole1, whole2, parents1, parents2)
        # self.assertEqual(str(ast1), str(ast2), f"String mismatch in {str(whole1)} vs {str(whole2)}:\n {str(ast1)} != {str(ast2)}")
        self.assertEqual(ast1, ast2, f"AST mismatch in {str(whole1)} vs {str(whole2)}:\n {str(ast1)} != {str(ast2)}")
        if hasattr(ast1, "location") and hasattr(ast2, "location"):
            self.assertEqual(ast1.location, ast2.location, f"Location mismatch in {str(whole1)} vs {str(whole2)}:\n {str(ast1.location)} != {str(ast2.location)}")
        parents1.pop()
        parents2.pop()

    def assertASTEqual(self, ast1, ast2):
        self._dispatch(ast1, ast2, ast1, ast2)
        self.assertEqual(ast1, ast2)

    def setUp(self):
        self.messages = []
        self.lib = Library(logger=lambda t, msg: self.messages.append((t, msg)))
        self.parser = parser.TreeSitterParser(self.lib)


    def test_tree_parse(self):
        code = textwrap.dedent("""\
            a := 1 :- b.
            b.
            """)
        code = bytes(code, "utf8")
        tree = self.parser._tree_parse(code)
        self.maxDiff = None
        self.assertEqual(
            format_ts_tree(tree, code).strip(),
            textwrap.dedent("""\
                └── source_file
                    ├── statement
                    │   └── assignment_rule
                    │       ├── simple_assignment
                    │       │   ├── term
                    │       │   │   └── function
                    │       │   │       └── name: identifier = a
                    │       │   ├── := = :=
                    │       │   └── term
                    │       │       └── number = 1
                    │       ├── :- = :-
                    │       └── body
                    │           ├── body_literal
                    │           │   └── symbolic_atom
                    │           │       └── name: identifier = b
                    │           └── . = .
                    └── statement
                        └── rule
                            ├── head
                            │   └── literal
                            │       └── symbolic_atom
                            │           └── name: identifier = b
                            └── . = .""").strip())

    def test_tree_parse_assignments(self):
        code = textwrap.dedent("""\
            a := 1 :- b.
            b.
            c := 2.
            """)
        code = bytes(code, "utf8")
        nodes = self.parser._tree_parse_assignments(code)
        self.assertEqual(len(nodes), 2)
        self.assertEqual(nodes[0].start_point, (0, 0))
        self.assertEqual(nodes[0].end_point, (0, 12))
        self.assertEqual(nodes[1].start_point, (2, 0))
        self.assertEqual(nodes[1].end_point, (2, 7))
        
        formatted_nodes = list(map(lambda n: format_ts_tree(n, code), nodes))
        self.assertEqual(
            formatted_nodes[0].strip(),
            textwrap.dedent("""\
                └── assignment_rule
                    ├── simple_assignment
                    │   ├── term
                    │   │   └── function
                    │   │       └── name: identifier = a
                    │   ├── := = :=
                    │   └── term
                    │       └── number = 1
                    ├── :- = :-
                    └── body
                        ├── body_literal
                        │   └── symbolic_atom
                        │       └── name: identifier = b
                        └── . = .
                    """).strip())
        

    def helper_parse(self, code, expected_clingo):
        clingo_rules = []
        try:
            rules = self.parser.parse(code)
            ast.parse_string(self.lib, expected_clingo, clingo_rules.append)
        except Exception as e:
            self.fail(f"Clingo messages: {self.messages}")
        string_rules = [c.strip() for c in code.strip().split("\n")]
        self.assertEqual(list(map(str, rules)), string_rules)
        assignment_rules = []
        previous_line = None
        previous_column = None
        for rule in rules:
            if previous_line is not None and previous_column is not None:
                self.assertEqual(rule.location.begin.line, previous_line+1)
                self.assertEqual(rule.location.begin.column, previous_column)
                self.assertEqual(rule.location.end.line, rule.location.begin.line)
                self.assertEqual(rule.location.end.column, rule.location.begin.column+len(str(rule)))
            previous_line = rule.location.begin.line
            previous_column = rule.location.begin.column
            if isinstance(rule, AssignmentRule):
                assignment_rules.append(rule)
                continue
            self.assertASTEqual(rules[1], clingo_rules[1])
            
        self.assertEqual(len(assignment_rules), 2)

    def test_parse_simple_assignment(self):
        code = textwrap.dedent("""\
            a := 1 :- b.
            b.
            c := 2.
            """)
        expected_clingo = textwrap.dedent("""\

            b.

        """)
        self.helper_parse(code, expected_clingo)
    

    def test_parse_aggregate_assignment(self):
        code = textwrap.dedent("""\
            a := 1 :- b.
            b.
            c := #sum{X: p(X); Y,X: q(X,Y)}.
            """)
        expected_clingo = textwrap.dedent("""\

            b.

        """)
        self.helper_parse(code, expected_clingo)

    def test_parse_choice_assignment(self):
        code = textwrap.dedent("""\
            a := 1 :- b.
            b.
            c in {X: p(X); Y,X: q(X,Y)}.
            """)
        expected_clingo = textwrap.dedent("""\

            b.

        """)
        self.helper_parse(code, expected_clingo)