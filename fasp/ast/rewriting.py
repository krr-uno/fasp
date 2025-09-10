from functools import singledispatchmethod
from itertools import chain
from typing import AbstractSet, Any, Iterable, List, Set, cast

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.symbol import Number, SymbolType

from fasp.ast.protecting import (
    COMPARISON_NAME,
    protect_comparisons,
    restore_comparisons,
)
from fasp.ast.syntax_checking import (
    ParsingException,
    SymbolSignature,
    SyntacticError,
    get_evaluable_functions,
)
from fasp.util.ast import (
    AST,
    ArgumentAST,
    BodyLiteralAST,
    FreshVariableGenerator,
    LiteralAST,
    StatementAST,
    TermAST,
    collect_variables,
    create_body_literal,
    create_literal,
    function_arguments,
    function_arguments_ast,
    is_function,
)


def normalize_ast(
    library: Library, statements: Iterable[StatementAST]
) -> Iterable[StatementAST]:
    rewrite_context = ast.RewriteContext(library)
    return restore_comparisons(
        library,
        chain.from_iterable(
            ast.rewrite_statement(rewrite_context, stmt)
            for stmt in protect_comparisons(library, statements)
        ),
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

    @singledispatchmethod
    def _dispatch(self, node: AST) -> AST | None:
        return node.transform(self.library, self.rewrite)

    @_dispatch.register
    def _(self, node: ast.LiteralComparison, *_args: Any, **_kwars: Any) -> AST | None:
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

    def rewrite(self, node: StatementAST, *args: Any, **kwargs: Any) -> StatementAST:
        result = self._dispatch(node, *args, **kwargs) or node
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
        library, ast.TermFunction(library, location, name, [args1tuple]),
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
        [ast.BodyAggregateElement(library, location, [return_variable], [lit2]),],
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
    ha_rewriter = HeadAggregateToBodyRewriteTransformer(library)
    statements = ha_rewriter.rewrite_statements(statements)
    if ha_rewriter.errors:
        raise ParsingException(ha_rewriter.errors)

    evaluable_functions = get_evaluable_functions(statements)
    transformer = NormalForm2PredicateTransformer(library, evaluable_functions, prefix)
    return (
        evaluable_functions,
        list(
            chain(
                (transformer.rewrite(statement) for statement in statements),
                functional_constraints(library, evaluable_functions, prefix),
            )
        ),
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
    # statements = normalize_ast(library, statements)
    evaluable_functions, statements = _functional2asp(library, statements, prefix)
    program = ast.Program(library)
    for statement in statements:
        program.add(statement)
    return evaluable_functions, program


class HeadAggregateToBodyRewriteTransformer:
    """
    Rewrites head aggregates of the form:
        f(X) = #agg{ ... } :- Body.
    into
        f(X) = W :- Body, W = #agg{ ... }.
    where W is a fresh variable not occurring in the original rule.

    Also collects syntactic errors if:
      - comparison symbol in the head-aggregate is not '='
      - the aggregate appears on the right-hand side (#agg{...} = f(X))
    """

    _SUPPORTED_FUNS = {
        ast.AggregateFunction.Sum,
        ast.AggregateFunction.Count,
        ast.AggregateFunction.Max,
        ast.AggregateFunction.Min,
    }

    def __init__(self, library: Library) -> None:
        self.library = library
        self.errors: list[SyntacticError] = []

    def rewrite_statements(self, statements: list[StatementAST]) -> list[StatementAST]:
        """Return new statements, recording errors in self.errors."""
        out: list[StatementAST] = []
        for st in statements:
            out.append(self._rewrite_statement(st))
        return out

    def _rewrite_statement(self, st: StatementAST) -> StatementAST:
        if not isinstance(st, ast.StatementRule):
            return st

        head = st.head
        if not isinstance(head, ast.HeadAggregate):
            return st

        # Detect whether the aggregate is on the left or right
        left_guard = head.left  # ast.LeftGuard | None
        right_guard = head.right  # ast.RightGuard | None

        # Reject aggregates on the right like #sum{...} = f(X)"
        if right_guard is not None:
            self._error(
                head.location,
                "Head aggregate cannot appear on the right-hand side of the assignment",
                type(head),
            )
            return st

        if left_guard is None:
            # Shouldn't happen for a well-formed head aggregate
            self._error(
                head.location,
                "Head aggregate is missing left guard of the assignment",
                type(head),
            )
            return st

        # The comparison must be equality
        if left_guard.relation != ast.Relation.Equal:
            # Suggesting the correct form in error message
            corrected_guard = ast.LeftGuard(
                self.library, left_guard.term, ast.Relation.Equal
            )
            corrected_head = ast.HeadAggregate(
                self.library,
                head.location,
                corrected_guard,
                head.function,
                head.elements,
                right_guard,
            )
            self._error(
                head.location,
                f'aggregates with comparisons cannot not be used in the head, found "{str(head)}", assignments are of the form "{str(corrected_head)}"',
                type(head),
            )
            return st

        lhs = left_guard.term
        # QUERY: Should all the types [except(TermSymbolic with SymbolType Number and String)] under util.ast.TermAST be allowed?
        if isinstance(lhs, ast.TermFunction):
            pass
        elif isinstance(lhs, ast.TermSymbolic):
            if lhs.symbol.type == SymbolType.Function:
                pass
            else:
                self._error(
                    head.location,
                    f"The left-hand side of an assignment must be a function term, "
                    f"found {type(lhs)} with symbol {lhs.symbol.type}",
                    type(head),
                )
                return st
        else:
            self._error(
                head.location,
                f"The left-hand side of an assignment must be a function term, "
                f"found {type(lhs)}",
                type(head),
            )
            return st

        # if isinstance(lhs, ast.TermSymbolic):
        #     if lhs.symbol.type in (SymbolType.Number, SymbolType.String):
        #         self._error(
        #             head.location,
        #             f"The left-hand side of an assignment must be a function term, "
        #             f"found {type(lhs)} with symbol {lhs.symbol.type}",
        #             type(head),
        #         )
        #         return st

        # Collect used variables in this rule to generate a fresh W
        used = collect_variables([st])
        gen = FreshVariableGenerator(used)
        W = gen.fresh_variable(self.library, head.location, "W")

        # Build the new head: f(Args) = W
        f_term = left_guard.term  # ast.TermFunction
        new_head_lit = ast.LiteralComparison(
            self.library,
            head.location,
            ast.Sign.NoSign,
            f_term,
            [ast.RightGuard(self.library, ast.Relation.Equal, W),],
        )
        new_head = ast.HeadSimpleLiteral(self.library, new_head_lit)

        # Convert head-aggregate elements to body-aggregate elements unchanged
        body_elems: list[ast.BodyAggregateElement] = []

        for el in head.elements:
            conditions: list[LiteralAST] = []
            if el.literal is not None:
                conditions.append(el.literal)
            conditions.extend(list(el.condition))
            # el: ast.HeadAggregateElement

            body_elems.append(
                ast.BodyAggregateElement(
                    self.library, el.location, list(el.tuple), conditions,
                )
            )

        # Build body aggregate W = #agg{ ... } (as a BodyAggregate with LeftGuard(W, Equal))
        body_agg = ast.BodyAggregate(
            self.library,
            head.location,
            ast.Sign.NoSign,
            ast.LeftGuard(self.library, W, ast.Relation.Equal),
            head.function,
            body_elems,
            None,
        )

        # Preserve the original body and append the equality-to-aggregate literal.
        new_body: list[BodyLiteralAST] = list(st.body) + [body_agg]

        # Return the rewritten rule.
        return ast.StatementRule(self.library, st.location, new_head, new_body)

    def _error(
        self, location: Location, message: str, information: type | None = None
    ) -> None:
        """
        Record a syntactic error at the given location with a structured message.
        """
        self.errors.append(SyntacticError(location, message, information))


class UnnestFunctions:
    """
    Recursively unnest evaluable functions in Clingo 6 AST.
    Fills `unnested_functions` with LiteralComparison objects of the form:
        original_term = FUNx
    """

    def __init__(
        self,
        lib: Library,
        evaluable_functions: Set[SymbolSignature],
        used_variable_names: Set[str],
    ):
        self.lib = lib
        self.evaluable_functions = evaluable_functions
        self.var_gen = FreshVariableGenerator(used_variable_names)
        self.unnested_functions: List[ast.LiteralComparison] = []

    def _is_evaluable(self, name: str, arity: int) -> bool:
        return SymbolSignature(name, arity) in self.evaluable_functions

    # ----------------- Term node handlers -----------------
    @singledispatchmethod
    def _unnest_term(self, term: ArgumentAST) -> ArgumentAST:
        # fallback: return as-is
        return term

    # @_unnest_term.register
    # def _(self, term: ast.TermVariable) -> ArgumentAST:
    #     return term

    @_unnest_term.register
    def _(self, term: ast.TermSymbolic) -> ArgumentAST:
        # Zero arity evaluable funcitons
        if term.symbol.type == SymbolType.Function and self._is_evaluable(
            term.symbol.name, 0
        ):
            fresh = self.var_gen.fresh_variable(self.lib, term.location, "FUN")
            comp = ast.LiteralComparison(
                self.lib,
                term.location,
                ast.Sign.NoSign,
                term,
                [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
            )
            self.unnested_functions.append(comp)
            return fresh
        return term

    @_unnest_term.register
    def _(self, term: ast.TermFunction) -> ArgumentAST:
        new_pool = [
            ast.ArgumentTuple(
                self.lib, tuple(self._unnest_term(a) for a in tup.arguments)
            )
            for tup in term.pool
        ]
        new_func = term.update(self.lib, pool=new_pool)
        if self._is_evaluable(term.name, sum(len(t.arguments) for t in new_pool)):
            fresh = self.var_gen.fresh_variable(self.lib, term.location, "FUN")
            comp = ast.LiteralComparison(
                self.lib,
                term.location,
                ast.Sign.NoSign,
                new_func,
                [ast.RightGuard(self.lib, ast.Relation.Equal, fresh)],
            )
            self.unnested_functions.append(comp)
            return fresh
        return new_func

    @_unnest_term.register
    def _(self, term: ast.TermTuple) -> ArgumentAST:
        new_pool = []
        for elem in term.pool:
            if isinstance(elem, ast.ArgumentTuple):
                new_pool.append(
                    ast.ArgumentTuple(
                        self.lib, tuple(self._unnest_term(a) for a in elem.arguments)
                    )
                )
            else:
                new_pool.append(ast.ArgumentTuple(self.lib, (self._unnest_term(elem),)))
        return term.update(self.lib, pool=new_pool)

    @_unnest_term.register
    def _(self, term: ast.TermAbsolute) -> ArgumentAST:
        new_pool = [self._unnest_term(t) for t in term.pool]
        return term.update(self.lib, pool=new_pool)

    @_unnest_term.register
    def _(self, term: ast.TermUnaryOperation) -> ArgumentAST:
        return term.update(self.lib, right=self._unnest_term(term.right))

    @_unnest_term.register
    def _(self, term: ast.TermBinaryOperation) -> ArgumentAST:
        return term.update(
            self.lib,
            left=self._unnest_term(term.left),
            right=self._unnest_term(term.right),
        )

    # ----------------- Generic transformer -----------------
    def _unnest(self, node: AST) -> AST:
        if isinstance(node, ArgumentAST):
            return self._unnest_term(node)
        return node.transform(self.lib, self._unnest) or node

    # ----------------- Statement transformer -----------------
    def transform_statement(self, st: StatementAST) -> StatementAST:
        return cast(StatementAST, self._unnest(st))

    def transform_statements(
        self, statements: List[StatementAST]
    ) -> List[StatementAST]:
        return [self.transform_statement(st) for st in statements]
