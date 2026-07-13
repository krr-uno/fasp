"""Executable audit of the documented intensional-function support matrix."""

import unittest

from clingo_funasp import ast

from funasp.ast import RewriteContext, parse_string, rewrite_statements
from funasp.core import Library
from funasp.util.ast import RewritingException


class TestAstInventory(unittest.TestCase):
    """Fail explicitly when a clingo_funasp upgrade adds an AST variant."""

    def test_statement_head_and_body_inventory(self):
        expected = {
            "BodyAggregate",
            "BodyAggregateElement",
            "BodyConditionalLiteral",
            "BodySetAggregate",
            "BodySimpleLiteral",
            "BodyTheoryAtom",
            "HeadAggregate",
            "HeadAggregateElement",
            "HeadConditionalLiteral",
            "HeadDisjunction",
            "HeadSetAggregate",
            "HeadSimpleLiteral",
            "HeadTheoryAtom",
            "StatementComment",
            "StatementConst",
            "StatementDefined",
            "StatementEdge",
            "StatementExternal",
            "StatementHeuristic",
            "StatementInclude",
            "StatementOptimize",
            "StatementParts",
            "StatementProgram",
            "StatementProject",
            "StatementProjectSignature",
            "StatementRule",
            "StatementScript",
            "StatementShow",
            "StatementShowNothing",
            "StatementShowSignature",
            "StatementTheory",
            "StatementWeakConstraint",
        }
        actual = {
            name
            for name in dir(ast)
            if name.startswith(("Statement", "Head", "Body"))
            and isinstance(getattr(ast, name), type)
        }
        self.assertEqual(actual, expected)


class TestUnsupportedDirectiveTerms(unittest.TestCase):
    def setUp(self):
        self.library = Library()

    def assert_rejected(self, directive: str, construct: str) -> None:
        program = f"f(a) := 1. {directive}"
        statements = parse_string(self.library, program)
        context = RewriteContext(self.library)
        with self.assertRaisesRegex(
            RewritingException,
            rf"intensional functions are not supported in {construct} statements: 'f\(a\)'",
        ):
            rewrite_statements(context, statements)

    def test_rough_terms_in_directives_are_rejected(self):
        cases = (
            ("#show p(f(a)) : q.", "Show"),
            ("#external p(f(a)) : q.", "External"),
            ("#heuristic p(f(a)) : q. [1,true]", "Heuristic"),
            ("#edge (f(a),b) : q.", "Edge"),
            ("#project p(f(a)) : q.", "Project"),
        )
        for directive, construct in cases:
            with self.subTest(construct=construct):
                self.assert_rejected(directive, construct)

    def test_functional_equation_in_show_condition_remains_supported(self):
        statements = parse_string(self.library, "f(a) := 1. #show p(X) : f(a) = X.")
        context = RewriteContext(self.library)
        rewritten = rewrite_statements(context, statements)
        text = "\n".join(
            str(item) for wrapper in rewritten for item in wrapper.rewritten
        )
        self.assertIn("#show p(X): Ff(a,X).", text)


class TestSupportedPositionsDoNotLeak(unittest.TestCase):
    """Ensure supported positions never retain an intensional Herbrand term."""

    def assert_no_rough_function_terms(self, program: str) -> None:
        library = Library()
        statements = parse_string(library, f"f(a) := 1. {program}")
        rewritten = rewrite_statements(RewriteContext(library), statements)
        leaked: list[str] = []

        def collect(node: object) -> None:
            if isinstance(node, ast.TermFunction) and node.name == "f":
                leaked.append(str(node))
                return
            if hasattr(node, "visit"):
                node.visit(collect)

        for wrapper in rewritten:
            for statement in wrapper.rewritten:
                statement.visit(collect)
        self.assertEqual(leaked, [])

    def test_supported_position_matrix(self):
        cases = (
            "p(f(a)) :- q.",
            "p :- q(f(a)) : r.",
            "p :- f(a) { q(X) }.",
            "p :- 0 < #count { X : q(X,f(a)) }.",
            "#minimize { f(a) : q }.",
            ":~ q(f(a)). [f(a)]",
        )
        for program in cases:
            with self.subTest(program=program):
                self.assert_no_rough_function_terms(program)

    def test_supported_negated_position_matrix(self):
        """Negated occurrences are lifted into auxiliary rules, never leaked."""
        cases = (
            "p :- q(X), not not r(f(X)).",
            "p :- q : not r(f(a)).",
            "p :- q(X) : r(X), not not s(f(X)).",
            "p :- 0 < #count { X : q(X), not r(f(X)) }.",
            "p :- 0 < #count { X : q(X), not f(X)+1 = 3 }.",
            "p :- 1 { not q(f(a)) : r }.",
            "1 = #count { X : not q(f(X)) : r(X) } :- s.",
            "1 = #count { X : not not q(f(X)) : r(X) } :- s.",
            "#minimize { 1,X : q(X), not r(f(X)) }.",
            ":~ q(X), not r(f(X)). [1,X]",
            ":~ q(X), not f(X)+1 = 3. [1,X]",
        )
        for program in cases:
            with self.subTest(program=program):
                self.assert_no_rough_function_terms(program)


if __name__ == "__main__":
    unittest.main()
