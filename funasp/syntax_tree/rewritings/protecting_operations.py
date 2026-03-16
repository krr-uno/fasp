from functools import singledispatch

from clingo import ast
from clingo.core import Library, Location
from clingo.symbol import Function, Number, Symbol, SymbolType

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

# def _symbol_to_operation(
#     library: Library,
#     location: Location,
#     symbol: Symbol,
#     op_name: str,
# ) -> ast.Term | None:
#     if symbol.type != SymbolType.Function:
#         return None

#     if symbol.name != op_name:
#         return None

#     args = list(symbol.arguments)

#     assert len(args) in (2, 3)

#     op_id_symbol = args[0]
#     assert op_id_symbol.type == SymbolType.Number
#     op_id = op_id_symbol.number

#     if len(args) == 2:
#         inner = ast.TermSymbolic(library, location, args[1])
#         return ast.TermAbsolute(library, location, [inner])

#     left = ast.TermSymbolic(library, location, args[1])
#     right = ast.TermSymbolic(library, location, args[2])

#     operator = INT_TO_BINARY_OPERATOR[op_id]

#     return ast.TermBinaryOperation(
#         library,
#         location,
#         left,
#         operator,
#         right,
#     )

# @singledispatch
# def _restore_operations(node: AST, context: RewriteContext) -> AST | None:
#     return node.transform(context.lib.library, _restore_operations, context)

# @_restore_operations.register
# def _(node: ast.TermSymbolic, context: RewriteContext) -> ast.Term | None:

#     library = context.lib.library
#     symbol = node.symbol

#     if symbol.type != SymbolType.Function:
#         return node

#     name = symbol.name
#     args = list(symbol.arguments)

#     new_args = []
#     contains_ast = False

#     # recurse into arguments
#     for arg in args:

#         if isinstance(arg, Symbol) and arg.type == SymbolType.Function:

#             term = ast.TermSymbolic(library, node.location, arg)

#             restored = _restore_operations(term, context)

#             if isinstance(restored, ast.TermSymbolic):
#                 new_args.append(restored.symbol)

#             else:
#                 contains_ast = True
#                 new_args.append(restored)

#         else:
#             new_args.append(arg)

#     # try converting this node to operation
#     op = _symbol_to_operation(
#         library,
#         node.location,
#         symbol,
#         context.prefix_protect_operation,
#     )

#     if op is not None:
#         return op

#     # rebuild function normally
#     if contains_ast:

#         ast_args = []

#         for arg in new_args:
#             if isinstance(arg, Symbol):
#                 ast_args.append(ast.TermSymbolic(library, node.location, arg))
#             else:
#                 ast_args.append(arg)

#         return ast.TermFunction(
#             library,
#             node.location,
#             name,
#             [ast.ArgumentTuple(library, ast_args)],
#         )

#     new_symbol = Function(library,name, new_args)

#     return ast.TermSymbolic(
#         library,
#         node.location,
#         new_symbol,
#     )


def restore_operations(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """
    Restore protected operations encoded as TermFunction(context.prefix_protect_operation, ...)
    back to TermBinaryOperation and TermAbsolute in the AST.
    """
    return statement
    # return statement.transform(context.lib.library, _restore_operations, context) or statement
