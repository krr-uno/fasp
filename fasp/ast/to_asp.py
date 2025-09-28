from functools import singledispatchmethod
from typing import AbstractSet, Iterable, cast

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.symbol import Number

from fasp.ast import (
    FASP_AST_T,
    AssignmentRule,
    FASP_Statement,
    HeadSimpleAssignment,
)
from fasp.ast.collectors import (
    SymbolSignature,
)
from fasp.ast.protecting import (  # protect_comparisons,; restore_comparisons,
    COMPARISON_NAME,
)
from fasp.util.ast import (
    BodyLiteralAST,
    StatementAST,
    create_body_literal,
    create_literal,
    function_arguments,
    function_arguments_ast,
    is_function,
)


class NormalForm2PredicateTransformer:
    """
    A class to transform a program in functional normal form into a regular program.
    """

    def __init__(
        self,
        library: Library,
        evaluable_functions: AbstractSet[SymbolSignature],
        prefix: str = "F",
        comparison_name: str = COMPARISON_NAME,
    ) -> None:
        """
        Initialize the transformer with the set of evaluable functions.
        """
        self.library = library
        self.evaluable_functions = evaluable_functions
        self.prefix = prefix
        self.comparison_name = comparison_name

    # @singledispatchmethod
    # def _dispatch(self, node: FASP_AST_T) -> FASP_AST_T | None:
    #     return node.transform(self.library, self.rewrite)

    # @_dispatch.register
    # def _(
    #     self, node: ast.LiteralComparison, *_args: Any, **_kwars: Any
    # ) -> ast.LiteralSymbolic | None:
    #     """
    #     Visit a Comparison node and transform it if it is an evaluable function.
    #     """
    #     assert len(node.right) >= 1, "Comparison must have at least one guard."
    #     if (
    #         not is_function(node.left)
    #         or len(node.right) != 1
    #         or node.right[0].relation != ast.Relation.Equal
    #     ):
    #         return None
    #     # assert type(node.left) in {ast.}
    #     name, arguments = function_arguments_ast(self.library, node.left)
    #     if SymbolSignature(name, len(arguments)) not in self.evaluable_functions:
    #         return None
    #     if __debug__:  # pragma: no cover
    #         if is_function(node.right[0].term):
    #             name2, arguments2 = function_arguments(node.right[0].term)
    #             signature = SymbolSignature(name2, len(arguments2))
    #             assert (
    #                 signature not in self.evaluable_functions
    #             ), "Guard term must not be an evaluable function."

    #     return ast.LiteralSymbolic(
    #         self.library,
    #         node.location,
    #         ast.Sign.NoSign,
    #         ast.TermFunction(
    #             self.library,
    #             node.left.location,
    #             f"{self.prefix}{name}",
    #             [ast.ArgumentTuple(self.library, [*arguments, node.right[0].term])],
    #         ),
    #     )

    @singledispatchmethod
    def _rewrite_head(self, node: HeadSimpleAssignment) -> ast.HeadSimpleLiteral:
        """
        Visit a HeadSimpleAssignment node and transform it if it is an evaluable function.
        """
        name, arguments = function_arguments_ast(self.library, node.assigned_function)
        assert (
            SymbolSignature(name, len(arguments)) in self.evaluable_functions
        ), f"Function {name}/{len(arguments)} not in evaluable functions {set(map(str, self.evaluable_functions))}."

        return ast.HeadSimpleLiteral(
            self.library,
            ast.LiteralSymbolic(
                self.library,
                node.location,
                ast.Sign.NoSign,
                ast.TermFunction(
                    self.library,
                    node.assigned_function.location,
                    f"{self.prefix}{name}",
                    [ast.ArgumentTuple(self.library, [*arguments, node.value])],
                ),
            ),
        )

    @singledispatchmethod
    def _dispach(self, node: FASP_AST_T) -> FASP_AST_T | None:
        """
        Visit a BodyLiteralAST node and transform it if it is an evaluable function.
        """
        return node.transform(self.library, self._dispach)

    @_dispach.register
    def _(self, node: ast.LiteralComparison) -> ast.LiteralSymbolic | None:
        assert len(node.right) >= 1, "Comparison must have at least one guard."
        if (
            not is_function(node.left)
            or len(node.right) != 1
            or node.right[0].relation != ast.Relation.Equal
        ):
            return None
        name, arguments = function_arguments_ast(self.library, node.left)
        if SymbolSignature(name, len(arguments)) not in self.evaluable_functions:
            return None
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
    

    @_dispach.register
    def _(self, node: AssignmentRule | ast.StatementRule) -> ast.StatementRule:
        """
        Visit an AssignmentRule node and transform it
        """
        new_rule = False
        if isinstance(node, AssignmentRule):
            head = self._rewrite_head(node.head)
            new_rule = True
        else:
            head = node.head
        body = []
        for lit in node.body:
            if (rewritten := self._dispach(lit)) is not None:
                body.append(rewritten)
                new_rule = True
            else:
                body.append(lit)
        if new_rule:
            return ast.StatementRule(self.library, node.location, head, body)
        return node

    def rewrite(self, node: FASP_Statement) -> StatementAST:
        result = self._dispach(node) or node
        return cast(StatementAST, result)


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
