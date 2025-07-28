from itertools import chain
from typing import AbstractSet, Any, Iterable
from clingo import ast, Number
from clingo.ast import parse_string


from fasp.ast.syntax_checking import SymbolSignature, get_evaluable_functions

from fasp.util.ast import EnhancedTransformer, create_literal


class NormalForm2PredicateTransformer(EnhancedTransformer):
    """
    A class to transform a program in functional normal form into a regular program.
    """

    def __init__(
        self, evaluable_functions: AbstractSet[SymbolSignature], prefix: str = "F"
    ) -> None:
        """
        Initialize the transformer with the set of evaluable functions.
        """
        self.evaluable_functions = evaluable_functions
        self.prefix = prefix

    def visit_Comparison(self, node: ast.AST, *args: Any, **kwargs: Any):
        """
        Visit a Comparison node and transform it if it is an evaluable function.
        """
        assert len(node.guards) >= 1, "Comparison must have at least one guard."
        if (
            len(node.guards) != 1
            or node.guards[0].comparison != ast.ComparisonOperator.Equal
            or node.term.ast_type != ast.ASTType.Function
            or SymbolSignature(node.term.name, len(node.term.arguments))
            not in self.evaluable_functions
        ):
            return node
        term = node.guards[0].term
        assert (
            term.ast_type != ast.ASTType.Function
            or SymbolSignature(term.name, len(term.arguments))
            not in self.evaluable_functions
        ), "Guard term must not be an evaluable function."
        return ast.Function(
            kwargs["location"],
            f"{self.prefix}{node.term.name}",
            [*node.term.arguments, node.guards[0].term],
            0,
        )


_POSITION = ast.Position("<functional>", 0, 0)
_LOCATION = ast.Location(_POSITION, _POSITION)


def _functional_constraint(function: SymbolSignature, prefix: str = "F") -> ast.AST:
    """
    Generate a functional constraint for a single evaluable function.

    Args:
        function (SymbolSignature): The evaluable function to generate the constraint for.
        prefix (str): The prefix to use for the function name.

    Returns:
        ast.AST: The functional constraint as an AST node.
    """
    anonymous_variable = ast.Variable(_LOCATION, "_")
    return_variable = ast.Variable(_LOCATION, "V")
    if function.arity == 0:
        args1 = [anonymous_variable]
        args2 = [return_variable]
    else:
        args1 = [ast.Variable(_LOCATION, f"X{i}") for i in range(function.arity)]
        args2 = list(args1)
        args1.append(anonymous_variable)
        args2.append(return_variable)
    name = f"{prefix}{function.name}"
    lit1 = create_literal(ast.Function(_LOCATION, name, args1, 0))
    lit2 = create_literal(ast.Function(_LOCATION, name, args2, 0))
    agg = ast.BodyAggregate(
        _LOCATION,
        ast.Guard(
            ast.ComparisonOperator.GreaterThan, ast.SymbolicTerm(_LOCATION, Number(1))
        ),
        ast.AggregateFunction.Count,
        [lit2],
        None,
    )
    head = create_literal(ast.BooleanConstant(False))
    return ast.Rule(_LOCATION, head, [lit1, agg])


def functional_constraints(
    evaluable_functions: Iterable[SymbolSignature], prefix: str = "F"
) -> Iterable[ast.AST]:
    """
    Generate functional constraints for evaluable functions.

    Args:
        evaluable_functions (Iterable[Function]): The set of evaluable functions.

    Returns:
        list[ast.AST]: A list of constraints for the functional normal form.
    """
    return (_functional_constraint(fun) for fun in evaluable_functions)


def functional2asp(
    program: list[ast.AST], prefix: str = "F"
) -> tuple[set[SymbolSignature], list[ast.AST]]:
    """
    Transform a program in functional normal form into a regular program.

    Args:
        program (Iterable[ast.AST]): The program to transform.

    Returns:
        Iterable[ast.AST]: The transformed program.
    """
    evaluable_functions = get_evaluable_functions(program)
    transformer = NormalForm2PredicateTransformer(evaluable_functions, prefix)
    return evaluable_functions, list(
        chain(
            (transformer.visit(statement) for statement in program),
            functional_constraints(evaluable_functions, prefix),
        )
    )
