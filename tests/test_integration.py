import textwrap
import unittest

from fasp.util.ast import ELibrary
from fasp.syntax_tree.parsing.parser import parse_string
from fasp.integration import FASPProgramTransformer

class TestFASPProgramTransformer(unittest.TestCase):
    def setUp(self):
        self.elib = ELibrary()

    def assertTransformEqual(self, program: str, expected_program: str | None, *, test_pipeline=10):
        program = textwrap.dedent(program).strip()
        expected_program = textwrap.dedent(expected_program).strip() if expected_program is not None else None

        statement_asts = parse_string(self.elib, program)
        transformer = FASPProgramTransformer(self.elib, statement_asts, prefix="F")
        transformed = transformer.transform(test_pipeline=test_pipeline)

        transformed_str = "\n".join([str(statement).strip() for statement in transformed][1:])

        self.assertEqual(transformed_str, expected_program)


    def test_choice_some_rewrite(self):
        self.assertTransformEqual(
            "a := #some{X: p(X)} :- q(X), r.",
            "{ a := X: p(X) } = 1 :- #count { X: p(X) } >= 1; q(X); r.",
            test_pipeline=2,
            )

    def test_assignment_aggregate_rewrite(self):
        """
        Test rewriting of assignment aggregates:
        """
        self.assertTransformEqual(
            "f(X) := #sum { Y: p(Y,Z) , q(X), r(X) } :- b(X,Z).",
            "f(X) := W :- b(X,Z); W = #sum { Y: p(Y,Z), q(X), r(X) }.",
            test_pipeline=2,
            )

    def test_combined_rewrite(self):
        """
        Combined program containing both a #some assignment and an assignment aggregate.
        Verifies pipeline chaining.
        """
        self.assertTransformEqual(
            """\
                a := #some{X: p(X)} :- s.
                f(X) := #count { Y: p(Y,Z) } :- b(X,Z).
            """,
            """\
                { ASS(a,X): p(X) } = 1 :- #count { X: p(X) } >= 1; s.
                ASS(f(X),W) :- b(X,Z); W = #count { Y: p(Y,Z) }.
            """,
            test_pipeline=4,
            )

    def test_choice_some_rewrite_context(self):
        self.assertTransformEqual(
            "a := #sum{X: p(X)} :- q(X), r.",
            "ASS(a,W) :- q(X); r; W = #sum { X: p(X) }.",
            test_pipeline=5,
            )

    def test_pool_rewrite(self):
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            "ASS(f(1;2),Y) :- g(Y).",
            test_pipeline=4,
            )

    def test_pool_rewrite_2(self):
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            """\
                ASS(f(1),Y) :- g(Y).
                ASS(f(2),Y) :- g(Y).
            """,
            test_pipeline=5,
            )

    def test_pool_restore_assignments(self):
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
             """\
                f(1) := Y :- g(Y).
                f(2) := Y :- g(Y).
            """,
            test_pipeline=7,
            )

    def test_comparison_rewrite(self):
        self.assertTransformEqual(
            "a=100.",
            "a=100.",
            test_pipeline=9,
            )

    def test_to_asp(self):
        self.assertTransformEqual(
            "f(1) := Y :- g(Y).",
            "Ff(1,Y) :- g(Y).",
        )

    def test_total_integration(self):
        self.assertTransformEqual(
            "score(X) := #sum{f(Y): f(p(Y)), q(X) } :- p.",
            "Fscore(X,W) :- p; W = #sum { f(Y): f(p(Y)), q(X) }.",
        )

    def test_total_integration_2(self):
        self.assertTransformEqual(
            "1 { sk(X, Y)  : skis(Y) } 1 :- sks(X).",
            "1 <= #count { 0,sk(X,Y): sk(X,Y): skis(Y) } <= 1 :- sks(X).",
        )

    def test_king(self):
        self.assertTransformEqual(
            """
            country(france).
            country(usa).
            person(felipe).

            {king(C,X) : person(X)}:- country(C).


            :- king(C1,X); king(C2,X); C1!=C2.
            """,
            """
            country(france).
            country(usa).
            person(felipe).
            #count { 0,king(C,X): king(C,X): person(X) } :- country(C).
            :- king(C1,X); king(C2,X); C1!=C2.
            """,
        )

    def test_head_aggregate_assignment(self):
        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            "#count{ 0,ASS(king(C),X): king(C) := X: person(X) } :- country(C).",
            test_pipeline=7,
        )

    def test_no_change(self):
        self.assertTransformEqual(
            "{ f(X) } :- g(Y).",
            "{ f(X) } :- g(Y).",
            )

    def test_to_asp_head_aggregate_assignment(self):
        self.assertTransformEqual(
            "#count { 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
            "#count { 0,ass(king(f(C)),X): Fking(g(C),h(X)): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
        )

        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            "#count { 0,ASS(king(C),X): Fking(C,X): person(X) } :- country(C).",
        )       