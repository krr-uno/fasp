from inspect import Signature
import textwrap
import unittest

from funasp.syntax_tree.types import SymbolSignature
from funasp.util.ast import ELibrary
from funasp.syntax_tree.parsing.parser import parse_string
from funasp.syntax_tree.rewritings.integration import transform_to_clingo_statements
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
        LOG: bool = False,
        evaluable_functions: set[str] | None = None,
    ):
        """Assert transform equal."""
        if evaluable_functions is None:
            evaluable_functions = set()

        evaluable_functions = {
            SymbolSignature(name, int(arity))
            for name, arity in (s.split("/") for s in evaluable_functions)
        }

        context = RewriteContext(self.elib, self.ctx.prefix_function, evaluable_functions = evaluable_functions)

        program = textwrap.dedent(program).strip()
        expected_program = (
            textwrap.dedent(expected_program).strip()
            if expected_program is not None
            else None
        )

        statement_asts = parse_string(self.elib, program)
        transformed = transform_to_clingo_statements(context, statement_asts)

        transformed_str = "\n".join(
            [str(statement).strip() for statement in transformed][1:]
        )

        self.assertEqual(transformed_str, expected_program)


    def test_to_asp(self):
        """Test to asp."""
        self.assertTransformEqual(
            "f(1) := Y :- g(Y).",
            """
            Ff(1,Y) :- g(Y).
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
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



    def test_fact(self):
        """Test fact."""
        self.assertTransformEqual(
            """
            c := 1.
            p(c).
            """,
            """
            Fc(1).
            p(FUN) :- Fc(FUN).
            :- Fc(_); 1 < #count { V: Fc(V) }.
            """
        )

    def test_pool(self):
        """Test pool."""
        self.assertTransformEqual(
            """
            f(1) := 2.
            p(f(a;b,c;d,e,f)).
            """,
            """
            Ff(1,2).
            p(FUN) :- Ff(a,FUN).
            p(FUN) :- f(b,c)=FUN.
            p(FUN) :- f(d,e,f)=FUN.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_assignment_rule(self):
        """Test assignment rule."""
        self.assertTransformEqual(
            """
            c := 1.
            a := b :- p(c).
            p(c).
            """,
            """
            Fc(1).
            Fa(b) :- p(FUN); Fc(FUN).
            p(FUN) :- Fc(FUN).
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Fc(_); 1 < #count { V: Fc(V) }.
            """
        )

    def test_head_aggregate_assignment2(self):
        """Test head aggregate assignment2."""
        self.assertTransformEqual(
            "{king(spain) := felipe}.",
            """
            #count { 0,Fking(spain,felipe): Fking(spain,felipe) }.
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )

    def test_no_change(self):
        """Test no change."""
        self.assertTransformEqual(
            "f(X) :- g(X).",
            "f(X) :- g(X).",
        )

    def test_aggregate(self):
        """Test aggregate."""
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            a := #sum{Y: f(X) = Y} :- q(X), r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            Fa(W) :- q(X); r; W = #sum { Y: Ff(X,Y) }.
            :- Fa(_); 1 < #count { V: Fa(V) }.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_choice_count(self):
        """Test choice count."""
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            #count { Y: p(X): f(X) = Y} :- q(X); r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            #count { Y: p(X): Ff(X,Y) } :- q(X); r.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            """,
        )

    def test_choice_count_Ass(self):
        """Test choice count Ass."""
        self.assertTransformEqual(
            """
            f(X) := Y :- p(X,Y).
            #count { Y: g(X) := Y: f(X) = Y} :- q(X); r.
            """,
            """
            Ff(X,Y) :- p(X,Y).
            #count { Y: Fg(X,Y): Ff(X,Y) } :- q(X); r.
            :- Ff(X0,_); 1 < #count { V: Ff(X0,V) }.
            :- Fg(X0,_); 1 < #count { V: Fg(X0,V) }.
            """,
        )

    def test_to_asp_head_aggregate_assignment(self):
        """Test to asp head aggregate assignment."""
        self.assertTransformEqual(
            "#count { 0,ass(king(f(C)),X): king(g(C)) := h(X): person(e(X)); ass(king(f(C)),X): f(X): person(e(X)) } :- country(C).",
            """
            #count { 0,ass(FUN,X): Fking(g(C),h(X)): person(e(X)), Fking(f(C),FUN); ass(FUN2,X): f(X): person(e(X)), Fking(f(C),FUN2) } :- country(C).
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )

        self.assertTransformEqual(
            "{king(C) := X: person(X)}:- country(C).",
            """
            #count { 0,Fking(C,X): Fking(C,X): person(X) } :- country(C).
            :- Fking(X0,_); 1 < #count { V: Fking(X0,V) }.
            """,
        )

    def test_fibo(self):
        """Test fibo."""
        self.assertTransformEqual(
            "fibo(X) := Y :- number(X); X>1; fibo(X-1) + fibo(X-2)=Y.",
            """
            Ffibo(X,Y) :- number(X); X>1; 1*__A_0+0=Y; Ffibo(1*X+(-1),FUN); Ffibo(1*X+(-2),FUN2); __A_0=FUN+FUN2.
            :- Ffibo(X0,_); 1 < #count { V: Ffibo(X0,V) }.
            """,
        )

    def test_company(self):
        """Test company."""
        self.assertTransformEqual(
            "controller(C3) := C1 :- company(C1), company(C3), #sum{controlsStk(C1,C2,C3), C2} > 50.",
            """
            Fcontroller(C3,C1) :- company(C1); company(C3); #sum { FUN,C2: FcontrolsStk(C1,C2,C3,FUN) } > 50.
            :- Fcontroller(X0,_); 1 < #count { V: Fcontroller(X0,V) }.
            :- FcontrolsStk(X0,X1,X2,_); 1 < #count { V: FcontrolsStk(X0,X1,X2,V) }.
            """,
            evaluable_functions={"controlsStk/3"},
        )

    def test_family_full(self):
        """Test family full."""
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
            Fn_orphan(W) :- W = #count { X: orphan(X) }.
            :- Ffather(X0,_); 1 < #count { V: Ffather(X0,V) }.
            :- Fmother(X0,_); 1 < #count { V: Fmother(X0,V) }.
            :- Fn_orphan(_); 1 < #count { V: Fn_orphan(V) }.
            """,
        )

    def test_unsafe(self):
        """Test unsafe."""
        with self.assertRaisesRegex(RuntimeError, r"\('rewriting failed', \[\(<clingo\.ast\.StatementRule object at 0x[0-9A-Fa-f]+>, RuntimeError\('rewriting failed'\)\)\]\)"):
            self.assertTransformEqual(
                """
                p(X) :- q(Y).
                """,
                ""
            )