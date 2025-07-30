
from functools import singledispatchmethod
from inspect import signature
from itertools import chain
import re
from typing import AbstractSet, Any, Iterable,cast
from clingo import ast
from clingo.core import Location, Position, Library
from clingo.symbol import Number



from fasp.ast.syntax_checking import SymbolSignature, get_evaluable_functions

from fasp.util.ast import create_literal, create_body_literal, AST, StatementAST, function_arguments_ast, function_arguments, is_function


class NormalForm2PredicateTransformer:
    """
    A class to transform a program in functional normal form into a regular program.
    """

    def __init__(
        self,
        library: Library,
        evaluable_functions: AbstractSet[SymbolSignature],
        prefix: str = "F",
    ) -> None:
        """
        Initialize the transformer with the set of evaluable functions.
        """
        self.library = library
        self.evaluable_functions = evaluable_functions
        self.prefix = prefix

    @singledispatchmethod
    def _dispatch(self, node) -> AST | None:
        return node.transform(self.library, self.rewrite)

    @_dispatch.register
    def _(self, node: ast.LiteralComparison, *args: Any, **kwargs: Any) -> AST | None:
        """
        Visit a Comparison node and transform it if it is an evaluable function.
        """
        assert len(node.right) >= 1, "Comparison must have at least one guard."
        if (
            not is_function(node.left)
            or len(node.right) != 1
            or node.right[0].relation != ast.Relation.Equal
        ):
            return None
        # assert type(node.left) in {ast.}
        name, arguments = function_arguments_ast(self.library, node.left)
        if SymbolSignature(name, len(arguments)) not in self.evaluable_functions:
            return None
        if __debug__:
            if is_function(node.right[0].term):
                name2, arguments2 = function_arguments(node.right[0].term)
                signature = SymbolSignature(name2, len(arguments2))
                assert (
                    signature not in self.evaluable_functions
                ), "Guard term must not be an evaluable function."

        return ast.LiteralSymbolic(
            self.library,
            node.location,
            ast.Sign.NoSign,
            ast.TermFunction(
                self.library,
                node.left.location,
                f"{self.prefix}{name}",
                [ast.ArgumentTuple(self.library, [*arguments, node.right[0].term])],
            ),
        )

    def rewrite(self, node: StatementAST, *args, **kwargs) -> StatementAST:
        result = self._dispatch(node, *args, **kwargs)
        if not result:
            return node
        return cast(StatementAST,result)


def _functional_constraint(
    library: Library, function: SymbolSignature, prefix: str = "F"
) -> ast.StatementRule:
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
    anonymous_variable = ast.TermVariable(library, location, "_")
    return_variable = ast.TermVariable(library, location, "V")
    if function.arity == 0:
        args1 = [anonymous_variable]
        args2 = [return_variable]
    else:
        args1 = [
            ast.TermVariable(library, location, f"X{i}") for i in range(function.arity)
        ]
        args2 = list(args1)
        args1.append(anonymous_variable)
        args2.append(return_variable)
    name = f"{prefix}{function.name}"
    args1tuple = ast.ArgumentTuple(library, args1)
    args2tuple = ast.ArgumentTuple(library, args2)
    lit1 = create_body_literal(
        library, 
        ast.TermFunction(library, location, name, [args1tuple]),
    )
    lit2 = create_literal(
        library, ast.TermFunction(library, location, name, [args2tuple])
    )
    agg = ast.BodyAggregate(
        library,
        location,
        ast.Sign.NoSign,
        ast.LeftGuard(
            library,
            ast.TermSymbolic(library, location, Number(library, 1)),
            ast.Relation.Greater,
        ),
        ast.AggregateFunction.Count,
        [
            ast.BodyAggregateElement(library, location, [return_variable], [lit2]),
        ],
        None,
    )
    head = ast.HeadSimpleLiteral(
        library, ast.LiteralBoolean(library, location, ast.Sign.NoSign, False)
    )
    return ast.StatementRule(library, location, head, [lit1, agg])


def functional_constraints(
    library: Library, evaluable_functions: Iterable[SymbolSignature], prefix: str = "F"
) -> Iterable[ast.StatementRule]:
    """
    Generate functional constraints for evaluable functions.

    Args:
        evaluable_functions (Iterable[Function]): The set of evaluable functions.

    Returns:
        list[ast.AST]: A list of constraints for the functional normal form.
    """
    return (_functional_constraint(library, fun, prefix) for fun in evaluable_functions)


def _functional2asp(
    library: Library, statements: list[StatementAST], prefix: str = "F"
) -> tuple[set[SymbolSignature], list[StatementAST]]:
    """
    Transform a program in functional normal form into a regular program.

    Args:
        program (Iterable[ast.AST]): The program to transform.

    Returns:
        Iterable[ast.AST]: The transformed program.
    """
    evaluable_functions = get_evaluable_functions(statements)
    transformer = NormalForm2PredicateTransformer(library, evaluable_functions, prefix)
    return evaluable_functions, list(
        chain(
            (transformer.rewrite(statement) for statement in statements),
            functional_constraints(library, evaluable_functions, prefix),
        )
    )

def functional2asp(
    library: Library, statements: list[StatementAST], prefix: str = "F"
) -> tuple[set[SymbolSignature], ast.Program]:
    """
    Transform a program in functional normal form into a regular program.

    Args:
        program (Iterable[ast.AST]): The program to transform.

    Returns:
        Iterable[ast.AST]: The transformed program.
    """
    evaluable_functions, statements = _functional2asp(library, statements, prefix)
    program = ast.Program(library)
    for statement in statements:
        program.add(statement)
    return evaluable_functions, program
