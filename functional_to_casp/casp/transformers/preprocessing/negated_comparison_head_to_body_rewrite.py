from functools import singledispatchmethod

from clingo import ast
from clingo.core import Library

from casp.transformers.preprocessing.base import PreprocessingTransformer
from casp.util.ast import StatementAST
from casp.util.util import extract_comparison_terms, negate_operator


class NegatedComparisonHeadToBodyTransformer(PreprocessingTransformer):
    """
    For each StatementRule:
      - If the head is a HeadDisjunction, pull out any LiteralComparison,
        negate their operator, wrap as body literals, and append to the body.
        Leave the remaining (non-comparison) elements in the head.
      - If the head is a HeadSimpleLiteral with a LiteralComparison, do the
        same but produce a 1-literal head.
      - Otherwise, leave the rule unchanged.
    """

    def __init__(self, lib: Library):
        self.lib = lib

    @singledispatchmethod
    def rewrite_rule(self, rule: StatementAST) -> StatementAST | None:
        return rule

    @rewrite_rule.register
    def _(self, rule: ast.StatementRule) -> ast.StatementRule | None:
        head = rule.head
        body = list(rule.body)

        # --- Case 1: Disjunctive head ---
        if isinstance(head, ast.HeadDisjunction):
            remaining = []
            for elem in head.elements:
                # Handle HeadConditionalLiteral
                lit = elem.literal if hasattr(elem, "literal") else elem
                if isinstance(lit, ast.LiteralComparison):
                    extracted = extract_comparison_terms(lit)
                    # if extracted:
                    # NOTE: Can `extracted` be None? If yes, then need to put this block in an if clause?
                    lhs_terms, op, rhs_terms = extracted
                    lhs_terms = tuple(lhs_terms)
                    rhs_terms = tuple(rhs_terms)
                    neg_op = negate_operator(op)
                    lhs = (
                        lhs_terms[0]
                        if len(lhs_terms) == 1
                        else ast.TermTuple(self.lib, lit.location, lhs_terms)
                    )
                    rhs = (
                        rhs_terms[0]
                        if len(rhs_terms) == 1
                        else ast.TermTuple(self.lib, lit.location, rhs_terms)
                    )
                    comp = ast.LiteralComparison(
                        self.lib,
                        lit.location,
                        ast.Sign.NoSign,
                        lhs,
                        [ast.RightGuard(self.lib, neg_op, rhs)],
                    )
                    body.append(ast.BodySimpleLiteral(self.lib, comp))
                    # else:
                    #     remaining.append(elem)
                else:
                    remaining.append(elem)
            new_head = ast.HeadDisjunction(self.lib, head.location, remaining)
            return ast.StatementRule(self.lib, rule.location, new_head, body)

        # --- Case 2: Single-literal head ---
        if isinstance(head, ast.HeadSimpleLiteral) and isinstance(
            getattr(head, "literal", None), ast.LiteralComparison
        ):
            comp_lit = head.literal
            assert isinstance(comp_lit, ast.LiteralComparison)  # For mypy: typecheck
            extracted = extract_comparison_terms(comp_lit)
            if extracted:
                lhs_terms, op, rhs_terms = extracted
                lhs_terms = tuple(lhs_terms)
                rhs_terms = tuple(rhs_terms)
                neg_op = negate_operator(op)
                lhs = (
                    lhs_terms[0]
                    if len(lhs_terms) == 1
                    else ast.TermTuple(self.lib, comp_lit.location, lhs_terms)
                )
                rhs = (
                    rhs_terms[0]
                    if len(rhs_terms) == 1
                    else ast.TermTuple(self.lib, comp_lit.location, rhs_terms)
                )
                comp = ast.LiteralComparison(
                    self.lib,
                    comp_lit.location,
                    ast.Sign.NoSign,
                    lhs,
                    [ast.RightGuard(self.lib, neg_op, rhs)],
                )
                body.append(ast.BodySimpleLiteral(self.lib, comp))
                # produce a rule with an empty head (constraint if body non-empty)
                empty_head = ast.HeadSimpleLiteral(
                    self.lib,
                    ast.LiteralBoolean(
                        self.lib, comp_lit.location, ast.Sign.NoSign, False
                    ),
                )
                return ast.StatementRule(self.lib, rule.location, empty_head, body)

        # --- Default: leave unchanged ---
        return None


# import clingo.ast as ast
# from fasp.patternFinders.ast_utils import ASTUtils
# class NegatedComparisonHeadToBodyTransformer(ast.Transformer):
#     """
#     For each Rule:
#       - If the head is a Disjunction, pull out any Comparison literals there,
#         negate their operator, wrap them as Literals, and append to the body.
#         Leave the remaining (non-Comparison) Disjunction elements in the head.
#       - If the head is a single Literal whose atom is a Comparison, do the
#         same but produce a 1-literal head.
#       - Otherwise, leave the rule unchanged.
#     """

#     def visit_Rule(self, node: ast.Rule) -> ast.AST:
#         head = node.head
#         body = list(node.body)  # ASTSequence → Python list

#         # --- Case 1: Disjunctive head ---
#         if head.ast_type == ast.ASTType.Disjunction:
#             remaining = []
#             for elem in head.elements:
#                 lit = elem.literal
#                 # only process if it's a bare Comparison
#                 if lit.atom.ast_type == ast.ASTType.Comparison:
#                     extracted = ASTUtils.extractComparisonTerm(lit.atom)
#                     if extracted:
#                         lhs_terms, op, rhs_terms = extracted
#                         neg_op = ASTUtils.negate_operator(op)
#                         # pick or wrap terms
#                         lhs = lhs_terms[0] if len(lhs_terms) == 1 else ast.Function(lit.location, "", lhs_terms, False)
#                         rhs = rhs_terms[0] if len(rhs_terms) == 1 else ast.Function(lit.location, "", rhs_terms, False)
#                         comp = ast.Comparison(
#                             term=lhs,
#                             guards=[ast.Guard(neg_op, rhs)]
#                         )
#                         body.append(ast.Literal(lit.location, ast.Sign.NoSign, comp))
#                     else:
#                         remaining.append(elem)
#                 else:
#                     remaining.append(elem)

#             # build new Disjunction head
#             new_head = ast.Disjunction(head.location, remaining)
#             return ast.Rule(node.location, new_head, body)

#         # --- Case 2: Single-literal head ---
#         if head.ast_type == ast.ASTType.Literal and head.atom.ast_type == ast.ASTType.Comparison:
#             extracted = ASTUtils.extractComparisonTerm(head.atom)
#             if extracted:
#                 lhs_terms, op, rhs_terms = extracted
#                 neg_op = ASTUtils.negate_operator(op)
#                 lhs = lhs_terms[0] if len(lhs_terms) == 1 else ast.Function(head.location, "", lhs_terms, False)
#                 rhs = rhs_terms[0] if len(rhs_terms) == 1 else ast.Function(head.location, "", rhs_terms, False)
#                 comp = ast.Comparison(
#                     location=head.location,
#                     term=lhs,
#                     guards=[ast.Guard(neg_op, rhs)]
#                 )
#                 body.append(ast.Literal(head.location, ast.Sign.NoSign, comp))
#                 # produce a rule with no head-comparison
#                 # (i.e. an empty “false head” becomes a constraint if body non-empty)
#                 return ast.Rule(node.location, ast.Literal(head.location, ast.Sign.NoSign, ast.Function(head.location, "", [], False)), body)

#         # --- Default: leave unchanged ---
#         return node
