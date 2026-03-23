from collections import defaultdict
from functools import singledispatchmethod
from typing import DefaultDict, Sequence

import clingo.ast as ast

import asp2fasp.util.util as util
from asp2fasp.util.ast import AST, StatementAST, TermAST
from asp2fasp.util.types import FPredicate


def split_program(
    statements: Sequence[ast.StatementRule],
) -> tuple[list[ast.StatementRule], dict[str, list[ast.StatementRule]]]:
    constraints: list[ast.StatementRule] = []
    definitions: DefaultDict[str, list[ast.StatementRule]] = defaultdict(list)

    for rule in statements:
        if util.is_constraint(rule.head):
            constraints.append(rule)
            continue
        splitter = ProgramSplitter()

        splitter.add_rule_definition(rule.head, rule, definitions)

    return constraints, dict(definitions)


class ProgramSplitter:
    def __init__(self) -> None:
        pass

    @singledispatchmethod
    def add_rule_definition(
        self,
        head: AST,
        rule: ast.StatementRule,
        definitions: DefaultDict[str, list[ast.StatementRule]],
    ) -> None:
        # Should not reach here since all possible head types should be covered, so we raise an error
        assert (
            False
        ), f"Undefined Head AST encountered {head}: {type(head)}."  # pragma: no cover
        return

    @add_rule_definition.register
    def _(
        self,
        head: ast.HeadSimpleLiteral,
        rule: ast.StatementRule,
        definitions: DefaultDict[str, list[ast.StatementRule]],
    ) -> None:
        if isinstance(head.literal, ast.LiteralSymbolic):
            key = predicate_key_from_literal_symbolic(head.literal)
            if key is not None:
                definitions[key].append(rule)

    @add_rule_definition.register
    def _(
        self,
        head: ast.HeadDisjunction,
        rule: ast.StatementRule,
        definitions: DefaultDict[str, list[ast.StatementRule]],
    ) -> None:
        for elem in head.elements:
            lit = elem.literal if isinstance(elem, ast.HeadConditionalLiteral) else elem
            if isinstance(lit, ast.LiteralSymbolic):
                key = predicate_key_from_literal_symbolic(lit)
                if key is not None:
                    definitions[key].append(rule)

    @add_rule_definition.register
    def _(
        self,
        head: ast.HeadSetAggregate,
        rule: ast.StatementRule,
        definitions: DefaultDict[str, list[ast.StatementRule]],
    ) -> None:
        self.add_aggregate_head_elements(head.elements, rule, definitions)

    @add_rule_definition.register
    def _(
        self,
        head: ast.HeadAggregate,
        rule: ast.StatementRule,
        definitions: DefaultDict[str, list[ast.StatementRule]],
    ) -> None:
        self.add_aggregate_head_elements(head.elements, rule, definitions)

    def add_aggregate_head_elements(
        self,
        elements: (
            Sequence[ast.HeadAggregateElement] | Sequence[ast.SetAggregateElement]
        ),
        rule: ast.StatementRule,
        definitions: DefaultDict[str, list[ast.StatementRule]],
    ) -> None:
        for elem in elements:
            if isinstance(elem.literal, ast.LiteralSymbolic):
                key = predicate_key_from_literal_symbolic(elem.literal)
                if key is not None:
                    definitions[key].append(rule)


def predicate_key_from_literal_symbolic(literal: ast.LiteralSymbolic) -> str | None:
    if isinstance(literal.atom, ast.TermFunction):
        name, args = util.function_arguments(literal.atom)
        return f"{name}/{len(args)}"
    return None
