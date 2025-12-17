from functools import singledispatchmethod
from typing import AbstractSet, Iterable, cast

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.symbol import Number

from fasp.syntax_tree._nodes import (
    FASP_AST_T,
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    ChoiceSomeAssignment,
    FASP_Statement,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from fasp.syntax_tree.collectors import (
    SymbolSignature,
)
from fasp.syntax_tree.protecting import (  # protect_comparisons,; restore_comparisons,
    COMPARISON_NAME,
)
from fasp.util.ast import (
    StatementAST,
    create_body_literal,
    create_literal,
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
    ) -> None:
        """
        Initialize the transformer with the set of evaluable functions.
        """
        self.library = library
        self.evaluable_functions = evaluable_functions
        self.prefix = prefix

    @singledispatchmethod
    def _rewrite_head(self, node: FASP_AST_T) -> FASP_AST_T:
        """
        Transforms functional head assignments like:
            f(X) := Y.
        or choices like:
            { f(X) := Y }.
        into predicate-based heads:
            f_f(X, Y).
        Default handler for head rewriting: return the node unchanged.
        """
        # Never called since all assignment based nodes have dispatchers.
        assert (
            False
        ), f"Unhandled Assignment node during Head AST rewriting: {type(node)}"  # pragma: no cover
        return node

    # Simple assignment f(X) := Y.
    @_rewrite_head.register
    def _(self, node: HeadSimpleAssignment) -> ast.HeadSimpleLiteral:
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

    # Choice { f(X) := Y }.
    @_rewrite_head.register
    def _(self, node: ChoiceAssignment) -> ast.HeadSetAggregate:
        """
        Visit a HeadSimpleAssignment node and transform it if it is an evaluable function.
        """
        elements = [self._rewrite_head(e) for e in node.elements]
        return ast.HeadSetAggregate(
            self.library,
            node.location,
            node.left,
            elements,
            node.right,
        )

    # Elements inside aggregates
    @_rewrite_head.register
    def _(self, node: AssignmentAggregateElement) -> ast.SetAggregateElement:
        assignment = self._rewrite_head(node.assignment)
        assert isinstance(
            assignment, ast.HeadSimpleLiteral
        ), f"Expected HeadSimpleLiteral after rewriting {node.assignment}"

        return ast.SetAggregateElement(
            self.library,
            node.location,
            assignment.literal,
            list(node.condition),
        )

    # HeadAggregateAssignment: f(X) := #sum{ f(X) : cond }.
    # HeadAggregateAssignment should be rewritten by normalize_assignment_aggregates

    @_rewrite_head.register
    def _(self, node: HeadAssignmentAggregate) -> ast.HeadAggregate:
        assert (
            False
        ), "HeadAggregateAssignment seen during Head AST rewrite during Normalization. This should not happen."

    @_rewrite_head.register
    def _(self, node: ChoiceSomeAssignment) -> ast.HeadAggregate:
        assert (
            False
        ), "ChoiceSomeAssignment seen during Head AST rewrite during Normalization. This should not happen."

    @singledispatchmethod
    def _dispatch(self, node: FASP_AST_T) -> FASP_AST_T | None:
        """
        Visit a BodyLiteralAST node and transform it if it is an evaluable function.
        """
        return node.transform(self.library, self._dispatch)

    @_dispatch.register
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

    @_dispatch.register
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
            if (rewritten := self._dispatch(lit)) is not None:
                body.append(rewritten)
                new_rule = True
            else:
                body.append(lit)
        if new_rule:
            return ast.StatementRule(self.library, node.location, head, body)
        assert isinstance(node, ast.StatementRule)
        return node

    def rewrite(self, node: FASP_Statement) -> StatementAST:
        result = self._dispatch(node) or node
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
