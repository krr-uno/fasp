from functools import singledispatchmethod
from typing import Any, Iterable

from clingo import ast
from clingo.symbol import SymbolType

from funasp.fun_ast._context import RewriteContext
from funasp.fun_ast.types import SymbolSignature
from funasp.util.ast import function_arguments_ast


def _restore_literal(
    context: RewriteContext,
    node: ast.LiteralSymbolic,
) -> ast.LiteralComparison | None:
    """Restore prefixed function literals into equalities for non-evaluable signatures."""
    atom = node.atom
    if isinstance(atom, ast.TermFunction):
        prefixed_name = atom.name
    elif isinstance(atom, ast.TermSymbolic) and atom.symbol.type == SymbolType.Function:
        prefixed_name = atom.symbol.name
    else:
        # TODO: Need to add test for this
        return None  # pragma: no cover

    prefix = context.prefix_function
    if not prefixed_name.startswith(prefix):
        return None

    base_name, arguments = function_arguments_ast(context.lib.library, atom)
    base_name = base_name[len(prefix) :]
    assert base_name is not None
    # if not base_name:
    #     return None

    arguments = list(arguments)
    assert len(arguments) >= 1

    original_arity = len(arguments) - 1
    if SymbolSignature(base_name, original_arity) in context.evaluable_functions:
        return None

    left = ast.TermFunction(
        context.lib.library,
        node.location,
        base_name,
        [ast.ArgumentTuple(context.lib.library, arguments[:-1])],
    )
    right_arg = (
        arguments[-1]
        if isinstance(arguments[-1], ast.Term)
        else ast.TermVariable(context.lib.library, node.location, "_", anonymous=True)
    )
    right = ast.RightGuard(
        context.lib.library,
        ast.Relation.Equal,
        right_arg,
    )
    return ast.LiteralComparison(
        context.lib.library,
        node.location,
        node.sign,
        left,
        [right],
    )


class _RestoreNonEvaluableFunctionsTransformer:
    """Restore non-evaluable prefixed literals throughout a statement AST."""

    def __init__(self, context: RewriteContext) -> None:
        """Initialize the transformer with the rewrite context."""
        self.context = context
        self.library = context.lib.library

    @singledispatchmethod
    def dispatch(self, node: Any) -> Any | None:  # pragma: no cover
        """Dispatch restoration recursively across AST nodes."""
        return node.transform(self.library, self.dispatch)

    @dispatch.register
    def _(self, node: ast.LiteralSymbolic) -> ast.LiteralComparison | None:
        """Restore a protected prefixed literal when it maps to a non-evaluable function."""
        new = _restore_literal(self.context, node)
        return new

    # @dispatch.register
    # def _(self, node: ast.HeadConditionalLiteral) -> ast.HeadConditionalLiteral | None:
    #     """Restore prefixed literals inside disjunctive head elements."""
    #     new_literal = self.dispatch(node.literal)
    #     if new_literal is None:
    #         return None
    #     assert isinstance(new_literal, ast.Literal)
    #     return node.update(self.library, literal=new_literal)

    # @dispatch.register
    # def _(self, node: ast.HeadDisjunction) -> ast.HeadDisjunction | None:
    #     """Restore prefixed literals in all disjunctive head elements."""
    #     new_elements: list[ast.HeadConditionalLiteral] = []
    #     changed = False
    #     for element in node.elements:
    #         rewritten = self.dispatch(element)
    #         if rewritten is None:
    #             assert isinstance(element, ast.HeadConditionalLiteral)
    #             new_elements.append(element)
    #         else:
    #             # Type narrowing: isinstance checks type but singledispatchmethod returns
    #             # a union of all handler return types, preventing mypy from narrowing.
    #             # The registered handler for HeadConditionalLiteral returns HeadConditionalLiteral | None.
    #             assert isinstance(rewritten, ast.HeadConditionalLiteral)
    #             new_elements.append(rewritten)
    #             changed = True
    #     if not changed:
    #         return None
    #     return node.update(self.library, elements=new_elements)

    @dispatch.register
    def _(self, node: ast.StatementRule) -> ast.StatementRule | None:
        """Restore prefixed literals in both rule heads and bodies."""
        rewritten_head = self.dispatch(node.head)
        if rewritten_head is None:
            new_head = node.head
            head_changed = False
        else:
            assert isinstance(rewritten_head, (ast.HeadLiteral, ast.HeadDisjunction))
            new_head = rewritten_head
            head_changed = True

        new_body: list[ast.BodyLiteral] = []
        changed = head_changed
        for literal in node.body:
            rewritten = self.dispatch(literal)
            if rewritten is None:
                new_body.append(literal)
            else:
                assert isinstance(rewritten, ast.BodyLiteral)
                new_body.append(rewritten)
                changed = True

        if not changed:
            return None
        return ast.StatementRule(self.library, node.location, new_head, new_body)

    # @dispatch.register
    # def _(self, node: ast.LiteralBoolean | ast.LiteralComparison) -> None:
    #     """Leave booleans and existing comparisons unchanged."""
    #     return None

    def rewrite(self, statement: ast.Statement) -> ast.Statement:
        """Apply restoration to one clingo statement."""
        rewritten = self.dispatch(statement)
        if rewritten is None:
            return statement
        assert isinstance(rewritten, ast.Statement)
        return rewritten


def restore_non_evaluable_functions(
    context: RewriteContext,
    statement: ast.Statement,
) -> ast.Statement:
    """Restore non-evaluable prefixed function literals in a statement."""
    return _RestoreNonEvaluableFunctionsTransformer(context).rewrite(statement)


def restore_non_evaluable_functions_list(
    context: RewriteContext,
    statements: Iterable[ast.Statement],
) -> list[ast.Statement]:
    """Restore non-evaluable prefixed function literals in a statement sequence."""
    return [
        restore_non_evaluable_functions(context, statement) for statement in statements
    ]
