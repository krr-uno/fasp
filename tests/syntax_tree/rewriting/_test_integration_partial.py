import textwrap
import unittest

from funasp.util.ast import ELibrary
from funasp.syntax_tree.parsing.parser import parse_string
from .integration_partial import (
    FASPProgramTransformer,
    PipelineStage,
)
from funasp.syntax_tree._context import RewriteContext


class TestFASPProgramTransformer(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures for each test."""
        self.elib = ELibrary()
        self.ctx = RewriteContext(self.elib, prefix_function="F")
        self.maxDiff = None  # Show full diff on assertion failure

    def assertTransformEqual(
        self,
        program: str,
        expected_program: str | None,
        *,
        test_pipeline=PipelineStage.TO_ASP,
        LOG: bool = False,
    ):
        """Assert transform equal."""
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
        """Test choice some rewrite."""
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
        """Test choice some rewrite context."""
        self.assertTransformEqual(
            "a := #sum{X: p(X)} :- q(X), r.",
            "ASS(a,W) :- q(X); r; W = #sum { X: p(X) }.",
            test_pipeline=PipelineStage.CLINGO_REWRITE,
        )

    def test_pool_rewrite(self):
        """Test pool rewrite."""
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            "ASS(f(1;2),Y) :- g(Y).",
            test_pipeline=PipelineStage.PROTECT_COMPARISONS,
        )

    def test_pool_rewrite_2(self):
        """Test pool rewrite 2."""
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            """\
                ASS(f(1),Y) :- g(Y).
                ASS(f(2),Y) :- g(Y).
            """,
            test_pipeline=PipelineStage.CLINGO_REWRITE,
        )

    def test_pool_restore_assignments(self):
        """Test pool restore assignments."""
        self.assertTransformEqual(
            "f(1;2) := Y :- g(Y).",
            """\
                f(1) := Y :- g(Y).
                f(2) := Y :- g(Y).
            """,
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_comparison_rewrite(self):
        """Test comparison rewrite."""
        self.assertTransformEqual(
            "a=100.",
            "a=100.",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_to_asp(self):
        """Test to asp."""
        self.assertTransformEqual(
            "f(1) := Y :- g(Y).",
            "Ff(1,Y) :- g(Y).",
        )

    def test_total_integration(self):
        """Test total integration."""
        self.assertTransformEqual(
            "1 { sk(X, Y)  : skis(Y) } 1 :- sks(X).",
            "1 <= #count { 0,sk(X,Y): sk(X,Y): skis(Y) } <= 1 :- sks(X).",
        )

    def test_king(self):
        """Test king."""
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
        """Test head aggregate assignment."""
        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            "#count{ 0,Fking(C,X): king(C) := X: person(X) } :- country(C).",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_head_aggregate_assignment2(self):
        """Test head aggregate assignment2."""
        self.assertTransformEqual(
            "{king(spain) := felipe}.",
            "#count{ 0,Fking(spain,felipe): king(spain) := felipe }.",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_fibo_comparitions(self):
        """Test fibo comparitions."""
        self.assertTransformEqual(
            "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
            "ASS(fibo(X),Y) :- number(X); CMP(X,(GRD(1,1),),0); CMP(fibo(X-1)+fibo(X-2),(GRD(0,Y),),0).",
            test_pipeline=PipelineStage.PROTECT_COMPARISONS,
        )

    def test_company_comparisons(self):
        """Test company comparisons."""
        self.assertTransformEqual(
            "controller(C3) := C1 :- company(C1), company(C3), #sum{controlsStk(C1,C2,C3), C2} > 50.",
            "ASS(controller(C3),C1) :- company(C1); company(C3); #sum { controlsStk(C1,C2,C3),C2 } > 50.",
            test_pipeline=PipelineStage.PROTECT_COMPARISONS,
        )

    # def test_company_rewrite(self):
    #     self.assertTransformEqual(
    #         "controller(C3) := C1 :- company(C1), company(C3), #sum{controlsStk(C1,C2,C3), C2} > 50.",
    #         "ASS(controller(C3),C1) :- company(C1); company(C3); #sum { controlsStk(C1,C2,C3),C2 } > 50.",
    #         test_pipeline=PipelineStage.CLINGO_REWRITE,
    #     )

    # def test_fibo_clingo_rewrite(self):
    #     self.assertTransformEqual(
    #         "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
    #         "ASS(fibo(X),Y) :- number(X); CMP(X,(GRD(1,1),),0); CMP(fibo(X-1)+fibo(X-2),(GRD(0,Y),),0).",
    #         test_pipeline=PipelineStage.CLINGO_REWRITE,
    #     )

    # def test_fibo_unested(self):
    #     self.assertTransformEqual(
    #         "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
    #         "fibo(X) := Y :- number(X); X>1; FUN+FUN2=Y; fibo(X-1)=FUN; fibo(X-2)=FUN2.",
    #         test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
    #     )


    # def test_fibo2(self):
    #     self.assertTransformEqual(
    #         "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
    #         "Ffibo(X,Y) :- number(X); X>1; FUN+FUN2=Y; Ffibo(X-1,FUN); Ffibo(X-2,FUN2).",
    #         test_pipeline=PipelineStage.TO_ASP,
    #     )

    # def test_fibo3(self):
    #     self.assertTransformEqual(
    #         "fibo(X) := fibo(X-1) + fibo(X-2) :- number(X); X>1.",
    #         "fibo(X) := FUN+FUN2 :- number(X); X>1; fibo(X-1)=FUN; fibo(X-2)=FUN2.",
    #         test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
    #     )

    def test_king0(self):
        """Test king0."""
        self.assertTransformEqual(
            "{ f(X) := Y: p(X,Y) } = 1.",
            "#count{ 0,Ff(X,Y): f(X) := Y: p(X,Y) } = 1.",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_basic_negated_literals(self):
        """Test basic negated literals."""
        self.assertTransformEqual(
            "a :- p(C); not country(C).",
            "a :- p(C); #false: country(C).",
            test_pipeline=PipelineStage.NEGATED_LITERALS,
        )

    def test_basic_negated_literals2(self):
        """Test basic negated literals2."""
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
        )

    def test_basic_negated_literals2b(self):
        """Test basic negated literals2b."""
        self.assertTransformEqual(
            """
            f(X) := 1 :- p(X).
            a :- p(C); #false: country(f(b)).
            """,
            """
            f(X) := 1 :- p(X).
            a :- p(*); #false: country(FUN), f(b)=FUN.
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
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
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
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
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

        self.assertTransformEqual(
            "a :- p(C); #false: country(C).",
            "a :- p(C); #false: country(C).",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS
        )

    def test_basic_negated_literals3(self):
        """Test basic negated literals3."""
        self.assertTransformEqual(
            "a :- p(C); not country(C).",
            "a :- p(C); #false: country(C).",
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_basic_negated_literals4(self):
        """Test basic negated literals4."""
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
        """Test negated literals."""
        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            "#count { 0,ASS(king(C),X): ASS(king(C),X): person(X) } :- country(C).",
            test_pipeline=PipelineStage.CLINGO_REWRITE,
        )

    def test_minimize(self):
        """Test minimize."""
        self.assertTransformEqual(
            """
            a := 1 :- p(X).
            #maximize { 1@0,f(X),a: p(X), f(a) }.
            """,
            """
            a := 1 :- p(X).
            :~ p(X); f(FUN2); a=FUN; a=FUN2. [-1@0,f(X),FUN]
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_weak_constraint(self):
        """Test weak constraint."""
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
        """Test weak constraint2."""
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
        """Test family."""
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
        """Test family right."""
        self.assertTransformEqual(
            "person(X) :- father(X)=_.",
            "person(X) :- father(X)=_.",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_family_left(self):
        """Test family left."""
        self.assertTransformEqual(
            "person(Y) :- father(_)=Y.",
            "person(Y) :- father(_)=Y.",
            test_pipeline=PipelineStage.RESTORE_ASSIGNMENTS,
        )

    def test_hamiltonian(self):
        """Test hamiltonian."""
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
        self.assertTransformEqual(
            """
            next(X) := #some{Y: edge(X,Y)} :- vertex(X).
            """,
            """
            #count{ 0,Fnext(X,Y): next(X) := Y: edge(X,Y) } = 1 :- vertex(X); #count { Y: edge(X,Y) } >= 1.
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

    def test_min_distance(self):
        """Test min distance."""
        self.assertTransformEqual(
            """
            dist(X,Y) := M+1 :- dist(X,Z)=M; edge(Z,Y); #false: edge(W,Y), dist(X,W)=N, N<M.
            """,
            """
            dist(X,Y) := 1*M+1 :- dist(X,Z)=M; edge(Z,Y); #false: edge(W,Y), dist(X,W)=N, N<M.
            """,
            test_pipeline=PipelineStage.UNNEST_FUNCTIONS,
        )

