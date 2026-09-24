"""Detect functional relations defined by an aggregate result.

Analyze original ASTs: preprocessing can substitute variables, split definitions,
or replace variables with projections, losing information needed by this proof.
Support a single positive or single-element choice rule with variable or
symbolic constant head arguments. Repeated result variables form tuple outputs.
All global variables used by the aggregate elements must be inputs;
local element variables do not contribute to the function's input signature.
"""

from collections import Counter
from collections.abc import Sequence

from clingo_funasp import ast
from clingo_funasp.core import Library

from funasp.asp2funasp.util.types import FPredicate, SymbolSignature
from funasp.ast import PARSER_PREFIX
from funasp.util.collectors import collect_predicates, collect_variables


class AggregateResultPatternFinder:
    """Find aggregate-result dependencies without changing source statements."""

    def __init__(self, library: Library) -> None:
        self.library = library

    def find(self, statements: Sequence[ast.Statement]) -> list[FPredicate]:
        """Find dependencies proved within one unparameterized program.

        Facts and other competing defining heads prevent single-definition
        inference. External declarations also prevent it. Parameterized program
        parts are excluded because their parameters can supply hidden inputs.
        """
        definitions: Counter[SymbolSignature] = Counter()
        rules: list[ast.StatementRule] = []
        for statement in statements:
            if isinstance(statement, ast.StatementProgram) and statement.arguments:
                return []
            if isinstance(statement, ast.StatementRule):
                definitions.update(collect_predicates(statement.head))
                rules.append(statement)
            elif isinstance(statement, ast.StatementExternal):
                literal = ast.LiteralSymbolic(
                    self.library, statement.location, ast.Sign.NoSign, statement.atom
                )
                # More than one marks the signature as unavailable regardless
                # of whether the external precedes or follows its defining rule.
                for signature in collect_predicates(literal):
                    definitions[signature] += 2

        found: list[FPredicate] = []
        for rule in rules:
            head = rule.head
            if isinstance(head, ast.HeadSetAggregate) and len(head.elements) == 1:
                literal = head.elements[0].literal
            elif isinstance(head, ast.HeadSimpleLiteral):
                literal = head.literal
            else:
                continue
            if not (
                isinstance(literal, ast.LiteralSymbolic)
                and literal.sign == ast.Sign.NoSign
                and isinstance(literal.atom, ast.TermFunction)
            ):
                continue
            atom = literal.atom
            if atom.name.startswith(PARSER_PREFIX) or len(atom.pool) != 1:
                continue
            arguments = atom.pool[0].arguments
            if not all(
                isinstance(arg, (ast.TermVariable, ast.TermSymbolic))
                for arg in arguments
            ):
                continue
            names = [
                arg.name if isinstance(arg, ast.TermVariable) else None
                for arg in arguments
            ]
            if "_" in names:
                continue
            signature = SymbolSignature(atom.name, len(names))
            if definitions[signature] != 1:
                continue
            outputs = self._output_positions(rule, names)
            if outputs:
                found.append(
                    FPredicate(
                        name=atom.name,
                        arity=len(names),
                        arguments=tuple(
                            i for i in range(len(names)) if i not in outputs
                        ),
                        values=outputs,
                        condition=[],
                    )
                )
        return found

    @staticmethod
    def _output_positions(
        rule: ast.StatementRule, names: list[str | None]
    ) -> tuple[int, ...]:
        """Find an equality-bound output whose aggregate context is fixed."""
        for index, literal in enumerate(rule.body):
            if (
                not isinstance(literal, ast.BodyAggregate)
                or literal.sign != ast.Sign.NoSign
            ):
                continue
            guards = [
                guard for guard in (literal.left, literal.right) if guard is not None
            ]
            global_variables = collect_variables(rule.head)
            for other_index, other in enumerate(rule.body):
                if other_index != index:
                    global_variables.update(collect_variables(other))
            for guard in guards:
                global_variables.update(collect_variables(guard.term))
            element_variables: set[str] = set()
            for element in literal.elements:
                element_variables.update(collect_variables(element))

            for guard in guards:
                if not (
                    guard.relation == ast.Relation.Equal
                    and isinstance(guard.term, ast.TermVariable)
                    and guard.term.name in names
                ):
                    continue
                inputs = set(names) - {guard.term.name}
                if element_variables.intersection(global_variables) <= inputs:
                    return tuple(
                        i for i, name in enumerate(names) if name == guard.term.name
                    )
        return ()
