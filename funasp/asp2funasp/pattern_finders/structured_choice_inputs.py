"""Conservative input inference for choice heads with constructor terms."""

from clingo_funasp import ast

from funasp.util.collectors import collect_variables


def constructor_variables(term: ast.TermOrProjection) -> set[str] | None:
    """Variables recoverable from a constructor value; None means unsupported.

    Arithmetic is deliberately excluded: knowing X+Y does not identify X and Y.
    Anonymous variables and projections likewise cannot identify a context.
    """
    if isinstance(term, ast.TermVariable):
        return None if term.name == "_" else {term.name}
    if isinstance(term, ast.TermSymbolic):
        return set()
    if (
        isinstance(term, (ast.TermFunction, ast.TermTuple))
        and len(term.pool) == 1
        and isinstance(term.pool[0], ast.ArgumentTuple)
    ):
        variables: set[str] = set()
        for argument in term.pool[0].arguments:
            nested = constructor_variables(argument)
            if nested is None:
                return None
            variables.update(nested)
        return variables
    return None


def structured_choice_inputs(
    rule: ast.StatementRule,
    literal: ast.Literal,
    outputs: list[int],
) -> list[int]:
    """Recover fixed inputs only when they retain the entire body context.

    Called as a fallback for the existing exactly-one detector. This does not
    flatten predicate arguments or change the existing simple-variable checks.
    """
    if not (
        isinstance(literal, ast.LiteralSymbolic)
        and isinstance(literal.atom, ast.TermFunction)
        and len(literal.atom.pool) == 1
    ):
        return []
    fixed: set[str] = set()
    context: set[str] = set()
    for body in rule.body:
        context.update(collect_variables(body))
        if (
            isinstance(body, ast.BodySimpleLiteral)
            and isinstance(body.literal, ast.LiteralSymbolic)
            and body.literal.sign == ast.Sign.NoSign
        ):
            variables = constructor_variables(body.literal.atom)
            if variables is not None:
                fixed.update(variables)

    inputs: list[int] = []
    retained: set[str] = set()
    for index, argument in enumerate(literal.atom.pool[0].arguments):
        variables = constructor_variables(argument)
        if variables is None:
            return []
        if index in outputs:
            if variables & context:
                return []
        else:
            if not variables <= fixed:
                return []
            inputs.append(index)
            retained.update(variables)
    if not context <= retained:
        return []
    return inputs
