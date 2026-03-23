from functools import singledispatch

from clingo import ast
from clingo.core import Location
from clingo.symbol import Number, Symbol, SymbolType

from funasp.syntax_tree._context import RewriteContext
from funasp.util.ast import AST

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
    """Protect arithmetic operation nodes by encoding them as synthetic function terms."""
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


def _symbol_tuple_to_term(
    symbol: Symbol,
    location: Location,
    context: RewriteContext,
) -> ast.TermTuple:
    """
    Convert a tuple Symbol into a TermTuple, recursively restoring elements.
    """
    library = context.lib.library

    args: list[ast.Term] = []

    for sym in symbol.arguments:
        # Wrap symbol into TermSymbolic
        term = ast.TermSymbolic(library, location, sym)

        # Recursively restore
        restored = _restore_operations(term, context)

        if restored is not None:
            assert isinstance(
                restored, ast.TermBinaryOperation | ast.TermAbsolute | ast.TermTuple
            )
            args.append(restored)
        else:
            args.append(term)
    arg_tuple = ast.ArgumentTuple(library, args)
    return ast.TermTuple(library, location, [arg_tuple])


def _symbol_to_term(
    location: Location,
    symbol: Symbol,
    context: RewriteContext,
) -> (
    ast.TermAbsolute | ast.TermBinaryOperation | ast.TermTuple | ast.TermFunction | None
):
    """
    Convert a Symbol representing OP(...) back into AST nodes.
    """
    library = context.lib.library
    if symbol.type == SymbolType.Tuple:
        return _symbol_tuple_to_term(symbol, location, context)
    if symbol.type != SymbolType.Function:
        return None

    if symbol.name != context.prefix_protect_operation:
        if not symbol.arguments:
            return None  # constant, leave unchanged

        args: list[ast.Term] = []

        for sym in symbol.arguments:
            term = ast.TermSymbolic(library, location, sym)
            restored = _restore_operations(term, context)

            if restored is not None:
                assert isinstance(restored, ast.Term)
                args.append(restored)
            else:
                args.append(term)

        arg_tuple = ast.ArgumentTuple(library, args)

        return ast.TermFunction(
            library,
            location,
            symbol.name,
            [arg_tuple],
        )

    symbol_args = list(symbol.arguments)

    assert len(symbol_args) in (2, 3)

    # First argument is operator ID
    op_id_sym = symbol_args[0]
    assert op_id_sym.type == SymbolType.Number
    op_id = op_id_sym.number

    # Helper: convert Symbol → TermSymbolic → recursively restore
    def _sym_to_term(
        sym: Symbol,
    ) -> (
        ast.TermSymbolic
        | ast.TermAbsolute
        | ast.TermBinaryOperation
        | ast.TermTuple
        | ast.TermFunction
    ):
        """Restore a nested symbol argument into the most specific term representation available."""
        term = ast.TermSymbolic(library, location, sym)
        restored = _restore_operations(term, context)
        if restored is not None:
            assert isinstance(
                restored, ast.TermBinaryOperation | ast.TermAbsolute | ast.TermTuple
            )
            return restored
        return term

    # Absolute
    if op_id == ABSOLUTE_INT:
        assert len(symbol_args) == 2

        inner = _sym_to_term(symbol_args[1])

        return ast.TermAbsolute(
            library,
            location,
            [inner],
        )

    # Binary operations
    assert len(symbol_args) == 3

    operator_type = INT_TO_BINARY_OPERATOR[op_id]

    left = _sym_to_term(symbol_args[1])
    right = _sym_to_term(symbol_args[2])

    return ast.TermBinaryOperation(
        library,
        location,
        left,
        operator_type,
        right,
    )


@singledispatch
def _restore_operations(node: AST, context: RewriteContext) -> AST | None:
    """Restore protected arithmetic operations inside the given AST node."""
    return node.transform(context.lib.library, _restore_operations, context)


@_restore_operations.register
def _(
    node: ast.TermSymbolic, context: RewriteContext
) -> (
    ast.TermAbsolute | ast.TermBinaryOperation | ast.TermTuple | ast.TermFunction | None
):
    """Restore protected operations stored inside a symbolic term when possible."""
    context.lib.library
    location = node.location
    symbol = node.symbol

    restored = _symbol_to_term(location, symbol, context)
    if restored is not None:
        return restored

    return None


def restore_operations(
    context: RewriteContext, statement: ast.Statement
) -> ast.Statement:
    """Restore protected arithmetic operations throughout the given statement."""
    return (
        statement.transform(context.lib.library, _restore_operations, context)
        or statement
    )
