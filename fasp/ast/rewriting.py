from itertools import chain
from typing import AbstractSet, Any, Iterable
from clingo import ast
from clingo.core import Location, Position, Library
from clingo.symbol import Number
from clingo.ast import parse_string


from fasp.ast.syntax_checking import SymbolSignature, get_evaluable_functions

from fasp.util.ast import HeadBodyVisitor, create_literal, AST, is_function


class NormalForm2PredicateTransformer(HeadBodyVisitor):
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

    def visit_Comparison(self, node: AST, *args: Any, **kwargs: Any):
        """
        Visit a Comparison node and transform it if it is an evaluable function.
        """
        assert len(node.guards) >= 1, "Comparison must have at least one guard."
        if (
            not is_function(node.left)
            or len(node.guards) != 1
            or node.right[0].comparison != ast.Relation.Equal
            or SymbolSignature(node.term.name, len(node.term.arguments)) not in self.evaluable_functions
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


def _functional_constraint(library: Library, function: SymbolSignature, location: Location, prefix: str = "F") -> ast.StatementRule:
    """
    Generate a functional constraint for a single evaluable function.

    Args:
        function (SymbolSignature): The evaluable function to generate the constraint for.
        prefix (str): The prefix to use for the function name.

    Returns:
        ast.AST: The functional constraint as an AST node.
    """
    position = Position(library, "<functional>", 0, 0)
    location = Location(position, position)
    anonymous_variable = ast.Variable(location, "_")
    return_variable = ast.Variable(location, "V")
    if function.arity == 0:
        args1 = [anonymous_variable]
        args2 = [return_variable]
    else:
        args1 = [ast.Variable(location, f"X{i}") for i in range(function.arity)]
        args2 = list(args1)
        args1.append(anonymous_variable)
        args2.append(return_variable)
    name = f"{prefix}{function.name}"
    lit1 = create_literal(ast.Function(location, name, args1, 0))
    lit2 = create_literal(ast.Function(location, name, args2, 0))
    agg = ast.BodyAggregate(
        library,
        location,
        ast.Sign.NoSign,
        ast.LeftGuard(
            library,
            ast.TermSymbolic(library, location, Number(1)),
            ast.Relation.GreaterThan,
        ),
        ast.AggregateFunction.Count,
        [
            ast.BodyAggregateElement(
                library, location, [return_variable], [lit2]
            ),
        ],
        None,
    )
    head = create_literal(ast.BooleanConstant(False))
    return ast.StatementRule(library, location, head, [lit1, agg])


def functional_constraints(
    library: Library,
    evaluable_functions: Iterable[SymbolSignature], prefix: str = "F"
) -> Iterable[ast.StatementRule]:
    """
    Generate functional constraints for evaluable functions.

    Args:
        evaluable_functions (Iterable[Function]): The set of evaluable functions.

    Returns:
        list[ast.AST]: A list of constraints for the functional normal form.
    """
    return (_functional_constraint(library, fun) for fun in evaluable_functions)


def functional2asp(
    library: Library,
    program: list[ast.StatementRule], prefix: str = "F"
) -> tuple[set[SymbolSignature], list[ast.StatementRule]]:
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
            (statement.transform(library, transformer) for statement in program),
            functional_constraints(evaluable_functions, prefix),
        )
    )
