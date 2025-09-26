import textwrap
from typing import Any, Iterable, Sequence
import unittest

from clingo import ast

from fasp.ast import AssignmentRule, HeadSimpleAssignment

# from fasp.ast.rewrittings.assigments import ParsingException
from fasp.util.ast import ParsingException
from fasp.util.ast import ELibrary
from fasp.util.ast import AST
from fasp.ast.tree_sitter import parser
from fasp.util.ast import parse_string


class TestParseAssignment(unittest.TestCase):

    def _dispatch(
        self,
        ast1: Any,
        ast2: Any,
        whole1: AST,
        whole2: AST,
        check_location: bool = True,
        parents1: list[AST] | None = None,
        parents2: list[AST] | None = None,
    ) -> None:
        if not isinstance(ast1, AST) or not isinstance(ast2, AST):
            return
        if parents1 is None:
            parents1 = []
        if parents2 is None:
            parents2 = []
        parents1.append((type(ast1), ast1))
        parents2.append((type(ast2), ast2))
        self.assertEqual(
            type(ast1),
            type(ast2),
            textwrap.dedent(
                f"""\
            Type mismatch in {str(whole1)} vs {str(whole2)}
            '{str(ast1)}' vs '{str(ast2)}'
            Parents1: {' > '.join(f'{t.__name__}[{str(a)}]' for t, a in parents1)}
            Parents2: {' > '.join(f'{t.__name__}[{str(a)}]' for t, a in parents2)}
            """
            ),
        )
        children = [m for m in dir(ast1) if not m.startswith("_")]
        for child in children:
            attr1 = getattr(ast1, child)
            attr2 = getattr(ast2, child)
            if callable(attr1) or callable(attr2):
                continue
            if isinstance(attr1, Iterable):
                for a1, a2 in zip(attr1, attr2):
                    self._dispatch(
                        a1, a2, whole1, whole2, check_location, parents1, parents2
                    )
            else:
                # print(f"Dispatching {child}: {attr1} vs {attr2}")
                self._dispatch(
                    attr1, attr2, whole1, whole2, check_location, parents1, parents2
                )
        # self.assertEqual(str(ast1), str(ast2), f"String mismatch in {str(whole1)} vs {str(whole2)}:\n {str(ast1)} != {str(ast2)}")
        self.assertEqual(
            ast1,
            ast2,
            f"AST mismatch in {str(whole1)} vs {str(whole2)}:\n {str(ast1)} != {str(ast2)}",
        )
        if check_location and hasattr(ast1, "location") and hasattr(ast2, "location"):
            self.assertEqual(
                ast1.location,
                ast2.location,
                f"Location mismatch in {str(whole1)} vs {str(whole2)}:\n {str(ast1.location)} != {str(ast2.location)}",
            )
        parents1.pop()
        parents2.pop()

    def assertASTEqual(self, ast1, ast2, check_location=True):
        if isinstance(ast1, Sequence):
            self.assertIsInstance(ast2, Sequence)
            self.assertEqual(
                len(ast1),
                len(ast2),
                f"Length mismatch: {list(map(str, ast1))} != {list(map(str, ast2))}",
            )
            for a1, a2 in zip(ast1, ast2):
                self._dispatch(a1, a2, a1, a2, check_location)
            return
        self._dispatch(ast1, ast2, ast1, ast2)
        self.assertEqual(ast1, ast2)

    def setUp(self):
        self.messages = []
        self.lib = ELibrary(logger=lambda t, msg: self.messages.append((t, msg)))
        self.parser = parser.TreeSitterParser(self.lib)

    def test_tree_parse_simple_assignment(self):
        code = textwrap.dedent(
            """\
            a := 1.
            """
        )
        rules = self.parser.parse(code)
        self.assertEqual(len(rules), 1)
        rule = rules[0]
        self.assertEqual(str(rule), "a := 1.")
        self.assertIsInstance(rule, AssignmentRule)
        self.assertEqual(str(rule.head), "a := 1")
        self.assertIsInstance(rule.head, HeadSimpleAssignment)
        self.assertEqual(str(rule.head.assigned_function), "a")
        self.assertIsInstance(
            rule.head.assigned_function, ast.TermFunction | ast.TermSymbolic
        )
        self.assertEqual(str(rule.head.value), "1")
        self.assertIsInstance(rule.head.value, ast.TermFunction | ast.TermSymbolic)

    def test_tree_parse_simple_assignments(self):
        code = textwrap.dedent(
            """\
            a := 1 :- b11; b12.
            b(X) := a+X :- b21(X); b22(X).
            """
        )
        rules = self.parser.parse(code)
        self.assertEqual(len(rules), 2)
        rule = rules[0]
        self.assertEqual(str(rule), "a := 1 :- b11; b12.")
        self.assertIsInstance(rule, AssignmentRule)
        self.assertEqual(str(rule.head), "a := 1")
        self.assertIsInstance(rule.head, HeadSimpleAssignment)
        self.assertEqual(str(rule.head.assigned_function), "a")
        self.assertIsInstance(
            rule.head.assigned_function, ast.TermFunction | ast.TermSymbolic
        )
        self.assertEqual(str(rule.head.value), "1")
        self.assertIsInstance(rule.head.value, ast.TermFunction | ast.TermSymbolic)
        rule = rules[1]
        self.assertEqual(str(rule), "b(X) := a+X :- b21(X); b22(X).")
        self.assertIsInstance(rule, AssignmentRule)
        self.assertEqual(str(rule.head), "b(X) := a+X")
        self.assertIsInstance(rule.head, HeadSimpleAssignment)
        self.assertEqual(str(rule.head.assigned_function), "b(X)")
        self.assertIsInstance(
            rule.head.assigned_function, ast.TermFunction | ast.TermSymbolic
        )
        self.assertEqual(str(rule.head.value), "a+X")
        self.assertIsInstance(rule.head.value, ast.TermBinaryOperation)

    def test_tree_parse_simple_assignments_with_clingo(self):
        code = textwrap.dedent(
            """\
            p(X) :- q(X).
            a := 1 :- b11; b12.
            r(X) :- t(X).
            b(X) := a+X :- b21(X); b22(X).
            t(1).
            """
        )
        rules = self.parser.parse(code)
        rule = rules[1]
        self.assertEqual(str(rule), "a := 1 :- b11; b12.")
        self.assertIsInstance(rule, AssignmentRule)
        self.assertEqual(str(rule.head), "a := 1")
        self.assertIsInstance(rule.head, HeadSimpleAssignment)
        self.assertEqual(str(rule.head.assigned_function), "a")
        self.assertIsInstance(
            rule.head.assigned_function, ast.TermFunction | ast.TermSymbolic
        )
        self.assertEqual(str(rule.head.value), "1")
        self.assertIsInstance(rule.head.value, ast.TermFunction | ast.TermSymbolic)
        rule = self.parser.parse(code)[3]
        self.assertEqual(str(rule), "b(X) := a+X :- b21(X); b22(X).")
        self.assertIsInstance(rule, AssignmentRule)
        self.assertEqual(str(rule.head), "b(X) := a+X")
        self.assertIsInstance(rule.head, HeadSimpleAssignment)
        self.assertEqual(str(rule.head.assigned_function), "b(X)")
        self.assertIsInstance(
            rule.head.assigned_function, ast.TermFunction | ast.TermSymbolic
        )
        self.assertEqual(str(rule.head.value), "a+X")
        self.assertIsInstance(rule.head.value, ast.TermBinaryOperation)
        clingo_rules = [rules[0], rules[2], rules[4]]
        self.assertTrue(all(isinstance(r, ast.StatementRule) for r in clingo_rules))
        expected_clingo = parse_string(
            self.lib,
            textwrap.dedent(
                """\
                p(X) :- q(X).

                r(X) :- t(X).

                t(1).
                """
            ),
        )
        self.assertASTEqual(clingo_rules, expected_clingo[1:])

    def test_parse_locations(self):
        code = textwrap.dedent(
            """\
            a := 1 :- b11; b12.
            b(X) := a+X :- b21(X); b22(X).  c := 3.
            """
        )
        rules = self.parser.parse(code)
        self.assertEqual(len(rules), 3)
        rule = rules[0]
        self.assertEqual(rule.location.begin.line, 1)
        self.assertEqual(rule.location.begin.column, 1)
        self.assertEqual(rule.location.end.line, 1)
        self.assertEqual(rule.location.end.column, 20)
        self.assertEqual(rule.head.location.begin.line, 1)
        self.assertEqual(rule.head.location.begin.column, 1)
        self.assertEqual(rule.head.location.end.line, 1)
        self.assertEqual(rule.head.location.end.column, 7)
        
        rule = rules[1]
        self.assertEqual(rule.location.begin.line, 2)
        self.assertEqual(rule.location.begin.column, 1)
        self.assertEqual(rule.location.end.line, 2)
        self.assertEqual(rule.location.end.column,  31)
        self.assertEqual(rule.head.location.begin.line, 2)
        self.assertEqual(rule.head.location.begin.column, 1)
        self.assertEqual(rule.head.location.end.line, 2)
        self.assertEqual(rule.head.location.end.column, 12)
    
        rule = rules[2]
        self.assertEqual(rule.location.begin.line, 2)
        self.assertEqual(rule.location.begin.column, 33)
        self.assertEqual(rule.location.end.line, 2)
        self.assertEqual(rule.location.end.column,  40)
        self.assertEqual(rule.head.location.begin.line, 2)
        self.assertEqual(rule.head.location.begin.column, 33)
        self.assertEqual(rule.head.location.end.line, 2)
        self.assertEqual(rule.head.location.end.column, 39)

    def assertEqualParse(self, code: str, expected: str | None = None):
        if not expected:
            expected = code
        rules = self.parser.parse(code)
        for rule in rules[1:]:
            self.assertIsInstance(rule, AssignmentRule | ast.StatementRule)
        lines = [sl for l in expected.strip().splitlines() if (sl := l.strip())]
        self.assertEqual(list(map(str, rules)), lines)

    def test_assignment_aggregate(self):
        self.assertEqualParse(
            textwrap.dedent(
                """\
                a := #sum{X: p(X); Y,X: q(X,Y)} :- b.
                f(Y) := #count{X: p(X)} :- b(Y).
                """
            )
        )

    def test_assignment_choice(self):
        self.assertEqualParse('{ a := 1 } :- b.')
        self.assertEqualParse('{ a := 1; b := 2 } :- c.')
        self.assertEqualParse('{ a := 1: p, q } :- c.')
        self.assertEqualParse('{ a := 1: p, q; b(X) := f(X): r, not s } :- c(X).')
        self.assertEqualParse('{ a := 1: p, q; p(X): r, not s } :- c(X).')
        self.assertEqualParse('1 <= { a := 1 } :- b.')
        self.assertEqualParse('{ a := 1 } <= 2 :- b.')
        self.assertEqualParse('1 <= { a := 1 } <= 3 :- b.')
        self.assertEqualParse('1{ a := 1 }3 :- b.', '1 <= { a := 1 } <= 3 :- b.')
        self.assertEqualParse('1 { a := 1: p, q; b(X) := f(X): r, not s } 5 :- c(X).', '1 <= { a := 1: p, q; b(X) := f(X): r, not s } <= 5 :- c(X).')

    def test_parse_merge(self):
        self.assertEqualParse(
            textwrap.dedent(
                """\
                a := 1 :- b.
                a := 2 :- b.
                b.
                c.
                """
            )
        )
        self.assertEqualParse(
            textwrap.dedent(
                """\
                b.
                c.
                a := 1 :- b.
                a := 2 :- b.
                """
            )
        )



    def test_parse_error_clingo(self):
        code = textwrap.dedent(
            """\
            a :- b.
            d
            d.
            d
            e :- f.
            """
        )
        with self.assertRaises(ParsingException) as cm:
            _ = self.parser.parse(code)
        errors = cm.exception.errors
        self.assertEqual(len(errors), 2)
        self.assertEqual(len(self.lib.error_messages), 0)
        self.assertEqual(errors[0].location.begin.line, 3)
        self.assertEqual(errors[1].location.begin.line, 5)

    def test_parse_error_assignment_number(self):
        code = textwrap.dedent(
            """\
            1 := 2.
            """
        )
        with self.assertRaises(ParsingException) as cm:
            _ = self.parser.parse(code)
        errors = cm.exception.errors
        self.assertEqual(len(errors), 1)
        self.assertEqual(len(self.lib.error_messages), 0)
        self.assertEqual(errors[0].location.begin.line, 1)

    def test_parse_error_assignment(self):
        code = textwrap.dedent(
            """\
            a := 2 :- b 
            c.
            """
        )
        with self.assertRaises(ParsingException) as cm:
            rules = self.parser.parse(code)
            print(rules)
        errors = cm.exception.errors
        self.assertEqual(len(errors), 1)
        self.assertEqual(len(self.lib.error_messages), 0)
        self.assertEqual(errors[0].location.begin.line, 1)
        self.assertEqual(errors[0].message, "a := 2 :- b  c.")

    # def test_tree_parse_error_assigned_is_not_function(self):
    #     code = textwrap.dedent(
    #         """\
    #         "a" := 1 :- b.
    #         1 := 1 :- b.
    #         (a,b) := 1 :- b.
    #         "a" := #sum{X: p(X)} :- b.
    #         1 := #sum{X: p(X)} :- b.
    #         (a,b) := #sum{X: p(X)} :- b.
    #         """
    #     )
    #     with self.assertRaises(ParsingException) as cm:
    #         _ = self.parser.parse(code)
    #     self.assertEqual(len(cm.exception.errors), 6)

    # def test_tree_parse_assignment_aggregate(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := #sum{X: p(X); Y,X: q(X,Y), not r(X)} :- b.
    #         """
    #     )
    #     rules = self.parser.parse(code)
    #     self.assertEqual(len(rules), 1)
    #     rule = rules[0]
    #     self.assertEqual(str(rule), "a := #sum{X: p(X); Y,X: q(X,Y), not r(X)} :- b.")
    #     self.assertIsInstance(rule, AssignmentRule)
    #     self.assertEqual(str(rule.head), "a := #sum{X: p(X); Y,X: q(X,Y), not r(X)}")
    #     self.assertIsInstance(rule.head, HeadAggregateAssignment)
    #     self.assertEqual(str(rule.head.assigned_function), "a")
    #     self.assertIsInstance(
    #         rule.head.assigned_function, ast.TermFunction | ast.TermSymbolic
    #     )
    #     self.assertEqual(rule.head.aggregate_function, ast.AggregateFunction.Sum)
    #     self.assertIsInstance(rule.head.elements, Sequence)
    #     self.assertEqual(len(rule.head.elements), 2)
    #     self.assertEqual(str(rule.head.elements[0]), "X: p(X)")
    #     self.assertIsInstance(rule.head.elements[0], ast.BodyAggregateElement)
    #     self.assertEqual(str(rule.head.elements[1]), "Y,X: q(X,Y), not r(X)")
    #     self.assertIsInstance(rule.head.elements[1], ast.BodyAggregateElement)
    #     self.assertEqual(len(rule.body), 1)
    #     self.assertEqual(str(rule.body[0]), "b")
    #     self.assertIsInstance(rule.body[0], ast.BodySimpleLiteral)

    # def test_tree_parse(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := 1 :- b.
    #         b.
    #         """
    #     )

    # code = bytes(code, "utf8")
    # tree = self.parser._tree_parse(code)
    # self.maxDiff = None
    # self.assertEqual(
    #     format_ts_tree(tree, code).strip(),
    #     textwrap.dedent(
    #         """\
    #         └── source_file
    #             ├── statement
    #             │   └── assignment_rule
    #             │       ├── simple_assignment
    #             │       │   ├── term
    #             │       │   │   └── function
    #             │       │   │       └── name: identifier = a
    #             │       │   ├── := = :=
    #             │       │   └── term
    #             │       │       └── number = 1
    #             │       ├── :- = :-
    #             │       └── body
    #             │           ├── body_literal
    #             │           │   └── symbolic_atom
    #             │           │       └── name: identifier = b
    #             │           └── . = .
    #             └── statement
    #                 └── rule
    #                     ├── head
    #                     │   └── literal
    #                     │       └── symbolic_atom
    #                     │           └── name: identifier = b
    #                     └── . = ."""
    #     ).strip(),
    # )

    # def test_tree_parse_assignments(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := 1 :- b.
    #         b.
    #         c := 2.
    #         """
    #     )
    #     code = bytes(code, "utf8")
    #     nodes = self.parser._tree_parse_assignments(code)
    #     self.assertEqual(len(nodes), 2)
    #     self.assertEqual(nodes[0].start_point, (0, 0))
    #     self.assertEqual(nodes[0].end_point, (0, 12))
    #     self.assertEqual(nodes[1].start_point, (2, 0))
    #     self.assertEqual(nodes[1].end_point, (2, 7))

    #     formatted_nodes = list(map(lambda n: format_ts_tree(n, code), nodes))
    #     self.assertEqual(
    #         formatted_nodes[0].strip(),
    #         textwrap.dedent(
    #             """\
    #             └── assignment_rule
    #                 ├── simple_assignment
    #                 │   ├── term
    #                 │   │   └── function
    #                 │   │       └── name: identifier = a
    #                 │   ├── := = :=
    #                 │   └── term
    #                 │       └── number = 1
    #                 ├── :- = :-
    #                 └── body
    #                     ├── body_literal
    #                     │   └── symbolic_atom
    #                     │       └── name: identifier = b
    #                     └── . = .
    #                 """
    #         ).strip(),
    #     )

    # def helper_parse(self, code, expected_clingo):
    #     clingo_rules = []
    #     try:
    #         rules = self.parser.parse(code)
    #         ast.parse_string(self.lib, expected_clingo, clingo_rules.append)
    #     except Exception as e:
    #         self.fail(f"Clingo messages: {self.messages}")
    #     string_rules = [c.strip() for c in code.strip().split("\n")]
    #     self.assertEqual(list(map(str, rules)), string_rules)
    #     assignment_rules = []
    #     previous_line = None
    #     previous_column = None
    #     for rule in rules:
    #         if previous_line is not None and previous_column is not None:
    #             self.assertEqual(rule.location.begin.line, previous_line + 1)
    #             self.assertEqual(rule.location.begin.column, previous_column)
    #             self.assertEqual(rule.location.end.line, rule.location.begin.line)
    #             self.assertEqual(
    #                 rule.location.end.column,
    #                 rule.location.begin.column + len(str(rule)),
    #             )
    #         previous_line = rule.location.begin.line
    #         previous_column = rule.location.begin.column
    #         if isinstance(rule, AssignmentRule):
    #             assignment_rules.append(rule)
    #             continue
    #         self.assertASTEqual(rules[1], clingo_rules[1])

    #     self.assertEqual(len(assignment_rules), 2)

    # def test_parse_simple_assignment(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := 1 :- b.
    #         b.
    #         c := 2.
    #         """
    #     )
    #     expected_clingo = textwrap.dedent(
    #         """\

    #         b.

    #     """
    #     )
    #     self.helper_parse(code, expected_clingo)

    # def test_parse_aggregate_assignment(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := 1 :- b.
    #         b.
    #         c := #sum{X: p(X); Y,X: q(X,Y)}.
    #         """
    #     )
    #     expected_clingo = textwrap.dedent(
    #         """\

    #         b.

    #     """
    #     )
    #     self.helper_parse(code, expected_clingo)

    # def test_parse_choice_assignment(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := 1 :- b.
    #         b.
    #         c in {X: p(X); Y,X: q(X,Y)}.
    #         """
    #     )
    #     expected_clingo = textwrap.dedent(
    #         """\

    #         b.

    #     """
    #     )
    #     self.helper_parse(code, expected_clingo)

    # def test_parse_missing_dot(self):
    #     code = textwrap.dedent(
    #         """\
    #         a :- b
    #         d.
    #         """
    #     )
    #     with self.assertRaises(RuntimeError) as cm:
    #         _ = self.parser.parse(code)
    #     print(cm.exception)
    #     print(self.parser.errors)
    #     print(self.messages)

    # def test_parse_missing_value(self):
    #     code = textwrap.dedent(
    #         """\
    #         a := :- b.
    #         d.
    #         c := 2.
    #         """
    #     )
    #     with self.assertRaises(RuntimeError) as cm:
    #         rules = self.parser.parse(code)
    #     print(cm.exception)
    #     print(self.parser.errors)
    #     print(self.messages)

    # def test_parse_incomplete_aggregate(self):
    #     code = textwrap.dedent(
    #         """\
    #         #sum :- b.
    #         d.
    #         """
    #     )
    #     with self.assertRaises(RuntimeError) as cm:
    #         rules = self.parser.parse(code)
    #     print(cm.exception)
    #     print(self.parser.errors)
    #     print(self.messages)

    # def test_parse_missing_function(self):
    #     code = textwrap.dedent(
    #         """\
    #         := 1 :- b.
    #         d.
    #         c := 2.
    #         """
    #     )
    #     with self.assertRaises(RuntimeError) as cm:
    #         rules = self.parser.parse(code)
    #     print(cm.exception)
    #     print(self.messages)
