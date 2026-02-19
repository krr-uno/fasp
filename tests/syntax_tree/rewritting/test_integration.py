import textwrap
import unittest

from fasp.util.ast import ELibrary
from fasp.syntax_tree.parsing.parser import parse_string
from fasp.syntax_tree.rewritings.integration import (
    FASPProgramTransformer,
    PipelineStage,
    transform_to_clingo_statements,
)
from fasp.syntax_tree._context import RewriteContext


class TestFASPProgramTransformer(unittest.TestCase):
    def setUp(self):
        self.elib = ELibrary()
        self.ctx = RewriteContext(self.elib, prefix="F")
        self.maxDiff = None  # Show full diff on assertion failure

    def assertTransformEqual(
        self,
        program: str,
        expected_program: str | None,
        *,
        test_pipeline=PipelineStage.TO_ASP,
        LOG: bool = False,
    ):
        program = textwrap.dedent(program).strip()
        expected_program = (
            textwrap.dedent(expected_program).strip()
            if expected_program is not None
            else None
        )

        statement_asts = parse_string(self.elib, program)
        transformer = FASPProgramTransformer(self.ctx, statement_asts)
        transformed = transformer.transform(stop_at=test_pipeline, LOG=LOG)


        transformed_str = "\n".join(
            [str(statement).strip() for statement in transformed][1:]
        )

        self.assertEqual(transformed_str, expected_program)

    def test_choice_some_rewrite(self):
        self.assertTransformEqual(
            "a := #some{X: p(X)} :- q(X), r.",
            "{ a := X: p(X) } = 1 :- #count { X: p(X) } >= 1; q(X); r.",
            test_pipeline=PipelineStage.NORMALIZE_ASSIGNMENT_AGGREGATES,
        )

    def test_assignment_aggregate_rewrite(self):
        """
        Test rewriting of assignment aggregates:
        """
        self.assertTransformEqual(
            "f(X) := #sum { Y: p(Y,Z) , q(X), r(X) } :- b(X,Z).",
            "f(X) := W :- b(X,Z); W = #sum { Y: p(Y,Z), q(X), r(X) }.",
            test_pipeline=PipelineStage.NORMALIZE_ASSIGNMENT_AGGREGATES,
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
            test_pipeline=PipelineStage.PROTECT_COMPARISONS,
        )

    def test_choice_some_rewrite_context(self):
        self.assertTransformEqual(
            "a := #sum{X: p(X)} :- q(X), r.",
            "ASS(a,W) :- q(X); r; W = #sum { X: p(X) }.",
            test_pipeline=PipelineStage.CLINGO_REWRITE,
        )

    def test_pool_rewrite(self):
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            "ASS(f(1;2),Y) :- g(Y).",
            test_pipeline=PipelineStage.PROTECT_COMPARISONS,
        )

    def test_pool_rewrite_2(self):
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            """\
                ASS(f(1),Y) :- g(Y).
                ASS(f(2),Y) :- g(Y).
            """,
            test_pipeline=PipelineStage.CLINGO_REWRITE,
        )

    def test_pool_restore_assignments(self):
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            """\
                f(1) := Y :- g(Y).
                f(2) := Y :- g(Y).
            """,
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_comparison_rewrite(self):
        self.assertTransformEqual(
            "a=100.",
            "a=100.",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_to_asp(self):
        self.assertTransformEqual(
            "f(1) := Y :- g(Y).",
            "Ff(1,Y) :- g(Y).",
        )

    def test_total_integration(self):
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
            "#count{ 0,Fking(C,X): king(C) := X: person(X) } :- country(C).",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_no_change(self):
        self.assertTransformEqual(
            "f(X) :- g(X).",
            "f(X) :- g(X).",
        )

    def test_aggregate(self):
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            a := #sum{Y: f(X) = Y} :- q(X), r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            Fa(W) :- q(X); r; W = #sum { Y: Ff(X,Y) }.
            """,
        )

    def test_choice_count(self):
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            #count { Y: p(X): f(X) = Y} :- q(X); r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            #count { Y: p(X): Ff(X,Y) } :- q(X); r.
            """,
        )

    def test_choice_count_Ass(self):
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            #count { Y: g(X) := Y: f(X) = Y} :- q(X); r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            #count { Y: Fg(X,Y): Ff(X,Y) } :- q(X); r.
            """,
        )

    def test_to_asp_head_aggregate_assignment(self):
        self.assertTransformEqual(
            "#count { 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
            "#count { 0,ass(FUN,X): Fking(g(C),h(X)): person(e(X)), Fking(f(C),FUN); ass(FUN2,X): f(X): person(e(X)), Fking(f(C),FUN2) } :- country(C).",
        )

        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            "#count { 0,Fking(C,X): Fking(C,X): person(X) } :- country(C).",
        )

    def test_fibo(self):
        self.assertTransformEqual(
            "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
            "fibo(X) := Y :- number(X); X>1; FUN+FUN2=Y; fibo(X-1)=FUN; fibo(X-2)=FUN2.",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )
        self.assertTransformEqual(
            "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
            "Ffibo(X,Y) :- number(X); X>1; FUN+FUN2=Y; Ffibo(X-1,FUN); Ffibo(X-2,FUN2).",
            test_pipeline=PipelineStage.TO_ASP,
        )

    def test_fibo2(self):
        self.assertTransformEqual(
            "fibo(X) := fibo(X-1) + fibo(X-2) :- number(X); X>1.",
            "fibo(X) := FUN+FUN2 :- number(X); X>1; fibo(X-1)=FUN; fibo(X-2)=FUN2.",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    # def test_king0(self):
    #     # self.assertTransformEqual(
    #     #     "{ f(X) := Y: p(X,Y) } = 1.",
    #     #     "#count{ 0,ASS(f(X),Y): f(X) := Y: p(X,Y) } = 1.",
    #     #     test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
    #     # )
    #     self.assertTransformEqual(
    #         "{ f(X) := Y: p(X,Y) } = N.",
    #         "{ ASS(f(X),Y): p(X,Y) } = N.",
    #         test_pipeline=PipelineStage.RESTORE_COMPARISONS,
    #     )
    #     # self.assertTransformEqual(
    #     #     "{ f(X) := Y: p(X,Y) } = N.",
    #     #     "{ f(X) := Y: p(X,Y) } = N.",
    #     #     test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
    #     # )
    #     # self.assertTransformEqual(
    #     #     "{ f(X):=Y: p(X,Y) } = N.",
    #     #     None,
    #     #     test_pipeline=PipelineStage.NEGATED_LITERALS,
    #     # )

    def test_basic_negated_literals(self):
        self.assertTransformEqual(
            "a :- p(C); not country(C).",
            "a :- p(C); #false: country(C).",
            test_pipeline=PipelineStage.NEGATED_LITERALS,
        )

    def test_basic_negated_literals2(self):
        self.assertTransformEqual(
            """
            f(X) := 1 :- p(X).
            a :- p(C); #false: country(f(C)).
            """,
            """
            f(X) := 1 :- p(X).
            a :- p(C); #false: country(FUN), f(C)=FUN.
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
            # LOG=True
        )

    def test_basic_negated_literals2b(self):
        self.assertTransformEqual(
            """
            f(X) := 1 :- p(X).
            a :- p(C); #false: country(f(b)).
            """,
            """
            f(X) := 1 :- p(X).
            a :- p(*); #false: country(FUN), f(b)=FUN.
            """,
            # a :- p(C); #false: country(FUN), f(b)=FUN.
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
            # LOG=True
        )
        self.assertTransformEqual(
            """
            b := 1 :- p(X).
            a :- p(C); #false: country(f(b)).
            """,
            """
            b := 1 :- p(X).
            a :- p(*); #false: country(f(FUN)), b=FUN.
            """,
            # a :- p(C); #false: country(FUN), f(b)=FUN.
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
            # LOG=True
        )
        self.assertTransformEqual(
            """
            b := 1 :- p(X).
            a :- p(C); #false: country(f(b,c)).
            """,
            """
            b := 1 :- p(X).
            a :- p(*); #false: country(f(FUN,c)), b=FUN.
            """,
            # a :- p(C); #false: country(FUN), f(b)=FUN.
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
            # LOG=True
        )

        # self.assertTransformEqual(
        #     "a :- p(C); #false: country(C).",
        #     "a :- p(C); #false: country(C).",
        #     test_pipeline=PipelineStage.UNNEST_FUNCTIONS
        # )

    def test_basic_negated_literals3(self):
        self.assertTransformEqual(
            "a :- p(C); not country(C).",
            "a :- p(C); #false: country(C).",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_basic_negated_literals4(self):
        self.assertTransformEqual(
            """
            a := 1 :- p(X).
            a :- p(*); #false: country(a).
            """,
            """
            a := 1 :- p(X).
            a :- p(*); #false: country(FUN), a=FUN.
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_negated_literals(self):
        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            "#count { 0,ASS(king(C),X): ASS(king(C),X): person(X) } :- country(C).",
            test_pipeline=PipelineStage.CLINGO_REWRITE,
        )

    def test_minimize(self):
        self.assertTransformEqual(
            """
            a := 1 :- p(X).
            #maximize { 1@0,f(X),a: p(X), f(a) }.
            """,
            # :~ p(X); f(a). [-1@0,f(X),a]
            # The maximize statement is changed into a weak constraint after clingo rewrite
            """
            a := 1 :- p(X).
            :~ p(X); f(FUN2); a=FUN; a=FUN2. [-1@0,f(X),FUN]
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_weak_constraint(self):
        self.assertTransformEqual(
            """
            a := 1 :- p(X).
            :~ p(X); f(FUN2); a=FUN; a=FUN2. [-1@0,f(X),FUN]
            """,
            """
            a := 1 :- p(X).
            :~ p(X); f(FUN2); a=FUN; a=FUN2. [-1@0,f(X),FUN]
            """,
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
            # Clingo Rewrite removes FUN variables.
        )

    def test_weak_constraint2(self):
        self.assertTransformEqual(
            """
            a := 1 :- p(X).
            :~ p(X); f(FUN2); a=FUN; a=FUN2. [-1@0,f(X),FUN]
            """,
            """
            a := 1 :- p(X).
            :~ p(X); f(FUN2); a=FUN; a=FUN2. [-1@0,f(X),FUN]
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_family(self):
        self.assertTransformEqual(
            "orphan(X) :- person(X), not father(X)=_, not mother(X)=_.",
            "orphan(X) :- person(X); #false: father(X)=_; #false: mother(X)=_.",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

        self.assertTransformEqual(
            "orphan(X) :- person(X), not father(X)=A, not mother(X)=A.",
            "orphan(X) :- person(X); #false: father(X)=A; #false: mother(X)=A.",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_family_right(self):
        self.assertTransformEqual(
            "person(X) :- father(X)=_.",
            "person(X) :- father(X)=_.",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )
    def test_family_left(self):
        self.assertTransformEqual(
            "person(Y) :- father(_)=Y.",
            "person(Y) :- father(_)=Y.",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_family_full(self):
        self.assertTransformEqual(
            """
            father(cain):=adam.
            father(abel):=adam.
            mother(cain):=eve.
            mother(abel):=eve.

            % person(father(X)).
            % person(mother(X)).
            person(Y) :- father(_)=Y.
            person(Y) :- mother(_)=Y.
            person(X) :- father(X)=_.
            person(X) :- mother(X)=_.
            % male(father(X)).
            % female(mother(X)).
            male(Y) :- father(_)=Y.
            female(Y) :- mother(_)=Y.

            orphan(X) :- person(X), not father(X)=_, not mother(X)=_.

            n_orphan := #count{X : orphan(X)}.
            """,
            """
            Ffather(cain,adam).
            Ffather(abel,adam).
            Fmother(cain,eve).
            Fmother(abel,eve).
            % person(mother(X)).
            #program base.
            person(Y) :- Ffather(_,Y).
            person(Y) :- Fmother(_,Y).
            person(X) :- Ffather(X,_).
            person(X) :- Fmother(X,_).
            % male(father(X)).
            % female(mother(X)).
            male(Y) :- Ffather(_,Y).
            female(Y) :- Fmother(_,Y).
            orphan(X) :- person(X); #false: Ffather(X,_); #false: Fmother(X,_).
            Fn_orphan(W) :- W = #count { X: orphan(X) }.""",
            test_pipeline=PipelineStage.TO_ASP,
        )

    def test_hamiltonian(self):
        self.assertTransformEqual(
            """
            next(X) := #some{Y: edge(X,Y)} :- vertex(X).
            """,
            """
            { next(X) := Y: edge(X,Y) } = 1 :- #count { Y: edge(X,Y) } >= 1; vertex(X).
            """,
            test_pipeline=PipelineStage.REWRITE_CHOICE_SOME,
        )
        self.assertTransformEqual(
            """
            next(X) := #some{Y: edge(X,Y)} :- vertex(X).
            """,
            """
            #count{ 0,Fnext(X,Y): next(X) := Y: edge(X,Y) } = 1 :- vertex(X); #count { Y: edge(X,Y) } >= 1.
            """,
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )
        # self.assertTransformEqual(
        #     """
        #     next(X) := #some{Y: edge(X,Y)} :- vertex(X).
        #     """,
        #     """
        #     #count{ 0,ASS(next(X),Y): next(X) := Y: edge(X,Y) } = 1 :- vertex(X); #count { Y: edge(X,Y) } >= 1.
        #     """,
        #     test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        # )