from functools import singledispatchmethod
from typing import AbstractSet, Iterable, cast

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.symbol import Number

from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree._nodes import (
    FASP_AST_T,
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    ChoiceSomeAssignment,
    FASP_Statement,
    HeadAggregateAssignment,
    HeadAggregateAssignmentElement,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from funasp.syntax_tree.types import (
    SymbolSignature,
)
from funasp.util.ast import (
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

    def _build_evaluable_function_to_term(
        self,
        assigned_function: ast.TermFunction | ast.TermSymbolic,
        value: ast.TermOrProjection,
        location: Location,
    ) -> ast.TermFunction:
        """Builds a function term given its assigned function and its value."""
        name, arguments = function_arguments_ast(self.library, assigned_function)
        assert (
            SymbolSignature(name, len(arguments)) in self.evaluable_functions
        ), f"Function {name}/{len(arguments)} not in evaluable functions {set(map(str, self.evaluable_functions))}."

        return ast.TermFunction(
            self.library,
            location,
            f"{self.prefix}{name}",
            [ast.ArgumentTuple(self.library, [*arguments, value])],
        )

        # return ast.TermFunction(
        #     self.library,
        #     location,
        #     f"{self.prefix}",
        #     [ast.ArgumentTuple(self.library, [assigned_function, value])],
        # )

    def function_to_literal(
        self,
        assigned_function: ast.TermFunction | ast.TermSymbolic,
        value: ast.TermOrProjection,
        location: Location,
    ) -> ast.LiteralSymbolic:
        """Wrap a rewritten function term in a symbolic literal."""

        return ast.LiteralSymbolic(
            self.library,
            location,
            ast.Sign.NoSign,
            self._build_evaluable_function_to_term(assigned_function, value, location),
        )

    @singledispatchmethod
    def _rewrite_head(self, node: FASP_AST_T) -> FASP_AST_T:
        """
        Transforms functional head assignments like:
            f(X) := Y.
        or choices like:
            { f(X) := Y }.
        into predicate-based heads:
            Ff(X, Y).
        Default handler: raises an assertion error for any unhandled node type,
        since all valid assignment head types have dedicated dispatchers.
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
        # name, arguments = function_arguments_ast(self.library, node.assigned_function)
        # assert (
        #     SymbolSignature(name, len(arguments)) in self.evaluable_functions
        # ), f"Function {name}/{len(arguments)} not in evaluable functions {set(map(str, self.evaluable_functions))}."

        literal = self.function_to_literal(
            node.assigned_function, node.value, node.location
        )
        return ast.HeadSimpleLiteral(
            self.library,
            literal,
        )

    # Choice { f(X) := Y }.
    @_rewrite_head.register
    def _(self, node: ChoiceAssignment) -> ast.HeadSetAggregate:
        """
        Visit a ChoiceAssignment node and rewrite its elements into a
        ``HeadSetAggregate`` of predicate-based literals.
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
        """Rewrite an assignment aggregate element into a set aggregate element."""
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

    @_rewrite_head.register
    def _(self, node: HeadAggregateAssignment) -> ast.HeadAggregate:
        """Rewrite a head aggregate assignment into a plain head aggregate."""
        new_elements = []
        for element in node.elements:
            if isinstance(element, HeadAggregateAssignmentElement):
                new_literal = self.function_to_literal(
                    element.assignment.assigned_function,
                    element.assignment.value,
                    element.assignment.location,
                )
                assert isinstance(
                    new_literal, ast.LiteralSymbolic
                ), f"Expected LiteralSymbolic after rewriting {element.assignment}"

                new_element = ast.HeadAggregateElement(
                    self.library,
                    element.location,
                    element.tuple,
                    new_literal,
                    element.condition,
                )
                new_elements.append(new_element)
            else:
                new_elements.append(element)

        return ast.HeadAggregate(
            self.library,
            node.location,
            node.left,
            node.function,
            new_elements,
            node.right,
        )
        # assignment = self._rewrite_head(node.assignment)
        # assert isinstance(
        #     assignment, ast.HeadSimpleLiteral
        # ), f"Expected HeadSimpleLiteral after rewriting {node.assignment}"

    # HeadAggregateAssignment: f(X) := #sum{ f(X) : cond }.
    # HeadAggregateAssignment should be rewritten by normalize_assignment_aggregates

    @_rewrite_head.register
    def _(self, node: HeadAssignmentAggregate) -> ast.HeadAggregate:
        """Reject unreduced assignment aggregates during head rewriting."""
        assert (
            False
        ), "HeadAggregateAssignment seen during Head AST rewrite during Normalization. This should not happen."

    @_rewrite_head.register
    def _(self, node: ChoiceSomeAssignment) -> ast.HeadAggregate:
        """Reject unreduced #some assignments during head rewriting."""
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
        """Rewrite functional equalities in literals into prefixed predicate literals."""
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
        return self.function_to_literal(node.left, node.right[0].term, node.location)

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
        new_head = self._dispatch(head)
        if new_head is not None:
            new_rule = True
            head = new_head
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

    def rewrite(self, node: FASP_Statement) -> ast.Statement:
        """Rewrite a normalized FASP statement into a plain clingo statement."""
        result = self._dispatch(node) or node
        return cast(ast.Statement, result)


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
            ast.Relation.Less,
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
    context: RewriteContext,
) -> Iterable[ast.StatementRule]:
    """
    Generate functional constraints for evaluable functions.

    Args:
        evaluable_functions (Iterable[Function]): The set of evaluable functions.

    Returns:
        list[ast.AST]: A list of constraints for the functional normal form.
    """
    return (
        _functional_constraint(context.lib.library, fun, context.prefix_function)
        for fun in sorted(context.evaluable_functions)
    )


def to_asp(
    context: RewriteContext,
    statement: FASP_Statement,
) -> ast.Statement:
    """Rewrite a single normalized FASP statement into plain ASP."""
    to_asp_transformer = NormalForm2PredicateTransformer(
        context.lib.library, context.evaluable_functions, context.prefix_function
    )
    return to_asp_transformer.rewrite(statement)


# def to_asp_list(
#     library: Library,
#     statements: Iterable[FASP_Statement],
#     evaluable_functions: AbstractSet[SymbolSignature],
#     prefix: str = "F",
# ) -> list[ast.Statement]:
#     to_asp_transformer = NormalForm2PredicateTransformer(
#         library, evaluable_functions, prefix
#     )
#     new_statements = [to_asp_transformer.rewrite(stmt) for stmt in statements]
#     return new_statements
