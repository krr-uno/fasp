from functools import singledispatch

from clingo import ast
from clingo.symbol import Number

from funasp.syntax_tree._context import RewriteContext
from funasp.util.ast import AST, function_arguments

INT_TO_BINARY_OPERATOR = [
    ast.BinaryOperator.And,  # 0
    ast.BinaryOperator.Division,  # 1
    ast.BinaryOperator.Minus,  # 2
    ast.BinaryOperator.Modulo,  # 3
    ast.BinaryOperator.Multiplication,  # 4
    ast.BinaryOperator.Or,  # 5
    ast.BinaryOperator.Plus,  # 6
    ast.BinaryOperator.Power,  # 7
    ast.BinaryOperator.Xor,  # 8
]

BINARY_OPERATOR_TO_INT = {op: i for i, op in enumerate(INT_TO_BINARY_OPERATOR)}

ABSOLUTE_INT = 9


# PROTECTION


@singledispatch
def _protect_operations(node: AST, context: RewriteContext) -> AST | None:
    return node.transform(context.lib.library, _protect_operations, context)


@_protect_operations.register
def _(node: ast.TermBinaryOperation, context: RewriteContext) -> ast.TermFunction:
    """
    Rewrite binary operations as OP(op_id, left, right).
    Children are already transformed by clingo's transform().
    """
    library = context.lib.library
    location = node.location

    op_id = BINARY_OPERATOR_TO_INT[node.operator_type]

    op_symbol = ast.TermSymbolic(
        library,
        location,
        Number(library, op_id),
    )

    # Recursively protect left and right terms
    left = node.left.transform(library, _protect_operations, context) or node.left
    right = node.right.transform(library, _protect_operations, context) or node.right

    argument_tuple = ast.ArgumentTuple(
        library,
        [
            op_symbol,
            left,
            right,
        ],
    )

    return ast.TermFunction(
        library,
        location,
        context.prefix_protect_operation,
        [argument_tuple],
    )


@_protect_operations.register
def _(node: ast.TermAbsolute, context: RewriteContext) -> ast.TermFunction:
    """
    Rewrite |x| as OP(9, x) after protecting x.
    """
    library = context.lib.library
    location = node.location

    assert len(node.pool) == 1

    # Recursively protect inner term
    inner = (
        node.pool[0].transform(library, _protect_operations, context) or node.pool[0]
    )

    op_symbol = ast.TermSymbolic(
        library,
        location,
        Number(library, ABSOLUTE_INT),
    )

    argument_tuple = ast.ArgumentTuple(
        library,
        [
            op_symbol,
            inner,
        ],
    )

    return ast.TermFunction(
        library,
        location,
        context.prefix_protect_operation,
        [argument_tuple],
    )


def protect_operations(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """
    Protect arithmetic/logical operations in a Clingo AST.
    """
    return (
        statement.transform(context.lib.library, _protect_operations, context)
        or statement
    )


# RESTORATION


# @singledispatch
# def _restore_operations(node: AST, context: RewriteContext) -> AST | None:
#     return node.transform(context.lib.library, _restore_operations, context)

# @_restore_operations.register
# def _(node: ast.TermSymbolic, context: RewriteContext) -> ast.Term | None:
#     library = context.lib.library
#     symbol = node.symbol

#     print(f"Restoring operations: visiting symbol {symbol} at {symbol.type} = Function: {SymbolType.Function}")
#     if symbol.type != SymbolType.Function:
#         return node

#     name = symbol.name
#     args = list(symbol.arguments)

#     # Recursively restore arguments first
#     new_args_ast = []
#     symbol_args = []

#     contains_ast = False

#     for arg in args:
#         if isinstance(arg, Symbol) and arg.type == SymbolType.Function:
#             term = ast.TermSymbolic(library, node.location, arg)
#             restored = _restore_operations(term, context)

#             if isinstance(restored, ast.TermSymbolic):
#                 symbol_args.append(restored.symbol)
#                 new_args_ast.append(restored)
#             else:
#                 contains_ast = True
#                 new_args_ast.append(restored)
#         else:
#             symbol_args.append(arg)
#             new_args_ast.append(ast.TermSymbolic(library, node.location, arg))

#     # If this is not OP, rebuild symbol
#     if name != context.prefix_protect_operation:

#         if contains_ast:
#             return ast.TermFunction(
#                 library,
#                 node.location,
#                 name,
#                 [ast.ArgumentTuple(library, new_args_ast)]
#             )

#         new_symbol = Function(library, name, symbol_args)
#         return ast.TermSymbolic(library, node.location, new_symbol)

#     # Now restore OP(...)
#     assert len(symbol_args) in (2,3)

#     op_symbol = symbol_args[0]
#     op_id = op_symbol.number

#     if len(symbol_args) == 2:
#         # Absolute
#         inner = ast.TermSymbolic(library, node.location, symbol_args[1])
#         return ast.TermAbsolute(
#             library,
#             node.location,
#             [inner]
#         )

#     # Binary
#     left = ast.TermSymbolic(library, node.location, symbol_args[1])
#     right = ast.TermSymbolic(library, node.location, symbol_args[2])

#     operator = INT_TO_BINARY_OPERATOR[op_id]

#     return ast.TermBinaryOperation(
#         library,
#         node.location,
#         left,
#         operator,
#         right,
#     )

# def restore_operations(
#     context: RewriteContext, statements: list[ast.Statement]
# ) -> list[ast.Statement]:
#     """
#     Restore protected operations encoded as TermFunction(context.prefix_protect_operation, ...)
#     back to TermBinaryOperation and TermAbsolute in the AST.
#     """
#     return statements
#     # return (
#     #     [statement.transform(context.lib.library, _restore_operations, context)
#     #     or statement for statement in statements]
#     # )
