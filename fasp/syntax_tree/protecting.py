from dataclasses import dataclass
from functools import singledispatchmethod
from typing import Any, Iterable, List, Optional, Sequence, cast

from clingo import ast
from clingo.core import Library, Location, Position
from clingo.symbol import Number, Symbol, SymbolType

from fasp.syntax_tree._nodes import (
    AssignmentAggregateElement,
    AssignmentRule,
    ChoiceAssignment,
    ChoiceSomeAssignment,
    FASP_Statement,
    HeadAssignmentAggregate,
    HeadSimpleAssignment,
)
from fasp.util.ast import (
    AST,
    AST_T,
    ArgumentAST,
    ELibrary,
    FunctionLikeAST,
    StatementAST,
    TermAST,
    function_arguments,
    is_function,
)

INT_TO_SIGN = [
    ast.Sign.NoSign,
    ast.Sign.Single,
    ast.Sign.Double,
]

INT_TO_RELATION = [
    ast.Relation.Equal,
    ast.Relation.Greater,
    ast.Relation.GreaterEqual,
    ast.Relation.Less,
    ast.Relation.LessEqual,
    ast.Relation.NotEqual,
]

SIGN_TO_INT = {r: i for i, r in enumerate(INT_TO_SIGN)}

RELATION_TO_INT = {r: i for i, r in enumerate(INT_TO_RELATION)}

COMPARISON_NAME = "CMP"
GUARD_NAME = "GRD"

ASSIGNMENT_NAME = "ASS"


class ComparisonProtector:
    """
    A class to protect comparisons in a Clingo AST.
    """

    def __init__(
        self,
        library: Library,
        comparison_name: str = COMPARISON_NAME,
        guard_name: str = GUARD_NAME,
    ):
        self.library = library
        self.comparison_name = comparison_name
        self.guard_name = guard_name
        position = Position(library, "<aux>", 0, 0)
        location = Location(position, position)
        self.sign_to_int = {
            r: ast.TermSymbolic(library, location, Number(library, n))
            for r, n in SIGN_TO_INT.items()
        }
        self.relation_to_int = {
            r: ast.TermSymbolic(library, location, Number(library, n))
            for r, n in RELATION_TO_INT.items()
        }

    def _guard_to_function(
        self, location: Location, guard: ast.RightGuard
    ) -> ast.TermFunction:
        arguments = ast.ArgumentTuple(
            self.library, [self.relation_to_int[guard.relation], guard.term]
        )
        return ast.TermFunction(self.library, location, self.guard_name, [arguments])

    def protect_comparison(
        self, comparison: ast.LiteralComparison
    ) -> ast.LiteralSymbolic:
        """
        Rewrites a LiteralComparison as a positive LiteralSymbolic of the form
            Comparison(left, right, sign)
        where right is a tuple of functions symbols Guard(relation, term)
        """
        location = comparison.location
        sign = self.sign_to_int[comparison.sign]
        left = comparison.left
        right = ast.TermTuple(
            self.library,
            location,
            [
                ast.ArgumentTuple(
                    self.library,
                    [self._guard_to_function(location, g) for g in comparison.right],
                )
            ],
        )
        atom = ast.TermFunction(
            self.library,
            location,
            self.comparison_name,
            [ast.ArgumentTuple(self.library, [left, right, sign])],
        )
        return ast.LiteralSymbolic(self.library, location, ast.Sign.NoSign, atom)

    def __call__(self, comparison: ast.LiteralComparison) -> ast.LiteralSymbolic:
        """
        Call method to protect a comparison.
        """
        return self.protect_comparison(comparison)


class _ComparisonProtectorTransformer:
    """
    A transformer to protect comparisons in a Clingo AST.
    """

    def __init__(self, library: Library):
        self.library = library
        self.protect_comparison = ComparisonProtector(library)

    @singledispatchmethod
    def dispatch(self, node: AST_T) -> AST_T:
        return node.transform(self.library, self.dispatch) or node

    @dispatch.register
    def _(self, node: ast.LiteralComparison) -> ast.LiteralSymbolic:
        return self.protect_comparison(node)

    @dispatch.register
    def _(
        self, node: ast.LiteralBoolean | ast.LiteralSymbolic
    ) -> ast.LiteralBoolean | ast.LiteralSymbolic:
        return node

    def rewrite(self, node: FASP_Statement) -> FASP_Statement:
        if not isinstance(node, ast.StatementRule):
            return node
        return node.transform(self.library, self.dispatch) or node


def protect_comparisons(
    library: Library, statements: Iterable[FASP_Statement]
) -> Iterable[FASP_Statement]:
    """
    Protect comparisons in a Clingo AST.

    Args:
        statements (Iterable[AST]): The AST statements to protect.

    Returns:
        Iterable[AST]: The protected AST statements.
    """
    transformer = _ComparisonProtectorTransformer(library)
    return (transformer.rewrite(statement) for statement in statements)


@dataclass
class RightGuard:
    """
    A class to represent a right guard in a comparison.

    Attributes:
        relation (ast.Relation): The relation of the guard.
        term (TermAST): The term associated with the guard.
    """

    relation: ast.Relation
    term: TermAST | Symbol

    def to_ast(self, library: Library, location: Location) -> ast.RightGuard:
        """
        Convert the RightGuard to an AST RightGuard.

        Args:
            library (Library): The Clingo library.

        Returns:
            ast.RightGuard: The AST representation of the right guard.
        """
        if isinstance(self.term, Symbol):  # pragma: no cover
            term = ast.TermSymbolic(library, location, self.term)
        else:
            term = self.term
        return ast.RightGuard(library, self.relation, term)


def _restore_guard_arguments(term: ast.TermFunction | Symbol) -> RightGuard:
    _, arguments = function_arguments(term)
    relation_int = arguments[0]
    term2 = arguments[1]
    if isinstance(relation_int, ast.TermSymbolic):
        relation_int = relation_int.symbol
    assert isinstance(
        relation_int, Symbol
    ), f"Expected a symbol, got {relation_int}: {type(relation_int)}"
    assert (
        relation_int.type == SymbolType.Number
    ), f"Expected a number, got {relation_int}: {relation_int.type}"
    # term2 = arguments[1]
    assert not isinstance(
        term2, ast.Projection
    ), f"Expected a non-tuple term, got {term2}: {type(term2)}"
    return RightGuard(INT_TO_RELATION[relation_int.number], term2)


def restore_comparison_arguments(
    arguments: Sequence[ArgumentAST] | Sequence[Symbol],
) -> tuple[ast.Sign, ArgumentAST | Symbol, list[RightGuard]]:
    assert (
        len(arguments) == 3
    ), f"Expected 3 arguments, got {len(arguments)}: {arguments}"
    left = arguments[0]
    right = arguments[1]
    sign = arguments[2]
    assert not isinstance(left, ast.Projection)
    assert isinstance(
        right, FunctionLikeAST
    ), f"Expected a tuple term, got {right}: {type(right)}"
    assert isinstance(
        sign, ast.TermSymbolic | Symbol
    ), f"Expected a tuple term, got {sign}: {type(sign)}"
    if isinstance(sign, ast.TermSymbolic):
        sign = sign.symbol
    sign = INT_TO_SIGN[sign.number]
    _, right = function_arguments(right)
    right = [
        _restore_guard_arguments(g)
        for g in cast(Sequence[ast.TermFunction] | Sequence[Symbol], right)
    ]
    return sign, left, right


def restore_comparison(
    library: Library,
    literal: ast.LiteralSymbolic,
    comparison_name: str = COMPARISON_NAME,
) -> ast.LiteralSymbolic | ast.LiteralComparison:
    atom = literal.atom
    assert is_function(atom)
    function_name, arguments = function_arguments(atom)
    if function_name != comparison_name:  # pragma: no cover
        return literal
    sign, left, right = restore_comparison_arguments(arguments)
    ast_right = [r.to_ast(library, literal.location) for r in right]
    if isinstance(left, Symbol):  # pragma: no cover
        left = ast.TermSymbolic(library, literal.location, left)
    assert not isinstance(
        left, ast.Projection
    ), f"Expected a non-projection term, got {left}: {type(left)}"
    return ast.LiteralComparison(library, literal.location, sign, left, ast_right)


class _ComparisonRestorationTransformer:
    """
    A transformer to restore comparisons in a Clingo AST.
    """

    def __init__(self, library: Library):
        self.library = library
        self.protect_comparison = ComparisonProtector(library)

    @singledispatchmethod
    def dispatch(self, node: AST) -> AST:  # pragma: no cover
        return node.transform(self.library, self.dispatch) or node

    @dispatch.register
    def _(  # pragma: no cover
        self, node: ast.LiteralSymbolic
    ) -> ast.LiteralSymbolic | ast.LiteralComparison:
        return restore_comparison(self.library, node)

    @dispatch.register
    def _(
        self, node: ast.LiteralBoolean | ast.LiteralComparison
    ) -> ast.LiteralBoolean | ast.LiteralComparison:
        return node

    def rewrite(self, node: StatementAST) -> StatementAST:
        if not isinstance(node, ast.StatementRule):
            return node
        return node.transform(self.library, self.dispatch) or node


def restore_comparisons(
    library: Library, statements: Iterable[StatementAST]
) -> Iterable[StatementAST]:
    """
    Protect comparisons in a Clingo AST.

    Args:
        statements (Iterable[AST]): The AST statements to protect.

    Returns:
        Iterable[AST]: The protected AST statements.
    """
    transformer = _ComparisonRestorationTransformer(library)

    return (transformer.rewrite(statement) for statement in statements)


# #########################################################################
class AssignmentProtector:
    """
    A class to protect assignments in FASP ASTs.
    """

    def __init__(self, library: Library, assignment_name: str = ASSIGNMENT_NAME):
        self.library = library
        self.assignment_name = assignment_name
        position = Position(library, "<aux>", 0, 0)
        self.location = Location(position, position)

    def protect_head_simple_assignment(
        self, node: HeadSimpleAssignment
    ) -> ast.LiteralSymbolic:
        left = node.assigned_function
        # right = node.value
        location = node.location

        # Build ASS(left, right)
        atom = ast.TermFunction(
            self.library,
            location,
            ASSIGNMENT_NAME,
            [ast.ArgumentTuple(self.library, [left, node.value])],
        )

        return ast.LiteralSymbolic(self.library, location, ast.Sign.NoSign, atom)

    # def protect_assignment_element(
    #     self, node: AssignmentAggregateElement
    # ) -> ast.LiteralSymbolic:
    #     """
    #     For an AssignmentAggregateElement the 'assignment' field is a HeadSimpleAssignment
    #     and we protect it similarly.
    #     """
    #     return self.protect_head_simple_assignment(node.assignment)


class _AssignmentProtectorTransformer:
    """
    A transformer to protect assignments in FASP ASTs.
    """

    def __init__(self, library: ELibrary):
        self.elib = library
        self.library = library.library

        self.protect_assignment = AssignmentProtector(self.library)

    @singledispatchmethod
    def dispatch(self, node: AST_T) -> AST_T:
        raise AssertionError(
            f"{(node).__class__.__name__} seen during assignment protection. Unhandled."
        )
        # if hasattr(node, "transform"):
        # # Recurse into children via their transform method.
        #     return node.transform(self.library, self.dispatch) or node
        # return node

    @dispatch.register
    def _(self, node: HeadSimpleAssignment) -> ast.LiteralSymbolic:
        return self.protect_assignment.protect_head_simple_assignment(node)

    # @dispatch.register
    # def _(self, node: AssignmentAggregateElement) -> ast.LiteralSymbolic:
    #     return self.protect_assignment.protect_assignment_element(node)

    # @dispatch.register
    # def _(self, node: ChoiceSomeAssignment) -> None:
    #     raise AssertionError(
    #         "ChoiceSomeAssignment seen during assignment protection. Unhandled."
    #     )

    def rewrite(self, node: FASP_Statement) -> StatementAST:
        if not isinstance(node, AssignmentRule):
            return node

        head = getattr(node, "head", None)
        body = list(getattr(node, "body", []))
        if isinstance(head, HeadSimpleAssignment):
            lit = self.dispatch(head)
            assert isinstance(lit, ast.LiteralSymbolic)

            new_head = ast.HeadSimpleLiteral(self.library, lit)
            return ast.StatementRule(self.library, node.location, new_head, body)

        if isinstance(head, ChoiceAssignment):
            new_elements = []
            for elem in head.elements:
                if isinstance(elem, AssignmentAggregateElement):
                    lit = self.dispatch(elem.assignment)
                    assert isinstance(lit, ast.LiteralSymbolic)
                    set_elem = ast.SetAggregateElement(
                        self.library,
                        elem.location,
                        lit,
                        elem.condition,
                    )
                    new_elements.append(set_elem)
                else:
                    new_elements.append(elem)

            new_head = ast.HeadSetAggregate(
                self.library,
                head.location,
                head.left_guard,
                new_elements,
                head.right_guard,
            )
            return ast.StatementRule(
                self.library,
                node.location,
                new_head,
                body,
            )
        # Raises assertion error from dispatch
        self.dispatch(head)
        # Should not happen
        return cast(StatementAST, node)  # pragma: no cover


def protect_assignments(
    library: ELibrary, statements: Iterable[FASP_Statement]
) -> Iterable[StatementAST]:
    """
    Protect assignments in a FASP AST (assignment-heads etc).

    Args:
        library (ELibrary): Clingo library with assignment node support.
        statements (Iterable[FASP_Statement]): Statements.

    Returns:
        Iterable[FASP_Statement]: Protected AST statements with assignments encoded as ASS(...).
    """
    transformer = _AssignmentProtectorTransformer(library)
    return (transformer.rewrite(statement) for statement in statements)


# RESTORATION: Rule with ASS(left, right) --> AssignmentRule


def _literal_is_protected_assignment(
    literal: ast.LiteralSymbolic, assignment_name: str = ASSIGNMENT_NAME
) -> Optional[Sequence[ArgumentAST] | Sequence[Symbol]]:
    """
    Checks if a literal is the protected assignment function or not.
    Returns None if not, else returns the protected assignment's arguments.
    """
    atom = literal.atom
    # QUESTION:
    if not is_function(atom):
        return None  # pragma: no cover

    name, arguments = function_arguments(atom)
    if name == assignment_name:
        return arguments
    return None


def restore_assignment_arguments(
    arguments: Sequence[ArgumentAST | Symbol],
) -> tuple[TermAST, TermAST]:
    """
    Extract (left, right) from ASS(left, right)
    """
    assert len(arguments) == 2, f"Expected 2 arguments in ASS(...), got: {arguments}"
    left, right = arguments

    # Ensure they are not Projections
    assert not isinstance(left, ast.Projection)
    assert not isinstance(right, ast.Projection)

    return cast(TermAST, left), cast(TermAST, right)


def _restore_assignment_term_function_to_head_simple_assignment(
    atom: ast.TermFunction,
) -> Optional[HeadSimpleAssignment]:
    # if not is_function(atom):
    #     return None  # pragma: no cover

    _, arguments = function_arguments(atom)

    left, right = restore_assignment_arguments(arguments)
    # assert isinstance(left, ast.TermFunction)
    return HeadSimpleAssignment(
        atom.location,
        cast(
            ast.TermFunction, left
        ),  # QUESTION: HeadSimpleAssignment only allows TermFunction as assigned function.
        right,
    )


def _restore_assignment_literal_to_head_simple_assignment(
    literal: ast.LiteralSymbolic,
    # assignment_name: str = ASSIGNMENT_NAME,
) -> Optional[HeadSimpleAssignment]:
    """
    Restore a protected assignment:
        ASS(left, right)  -->  HeadSimpleAssignment(left, right)
    """
    atom = literal.atom
    assert isinstance(atom, ast.TermFunction)
    return _restore_assignment_term_function_to_head_simple_assignment(atom)


class _AssignmentRestorationTransformer:
    """
    Transformer to restore ASS(...) symbolic literals to HeadSimpleAssignment
    """

    def __init__(self, library: ELibrary):
        self.elib = library
        self.library = library.library

    def _restore_set_aggregate_head(
        self, head: ast.HeadSetAggregate
    ) -> Optional[ChoiceAssignment]:
        """
        If head is a HeadSetAggregate with SetAggregateElement(s) where the element.literal is ASS(...),
        convert to a ChoiceAssignment with AssignmentAggregateElement(s).
        """
        new_elements: List[Any] = []
        any_converted = False

        for elem in head.elements:
            lit = elem.literal
            # check if it's ASS(...)
            if (
                isinstance(lit, ast.LiteralSymbolic)
                and _literal_is_protected_assignment(lit) != None
            ):
                head_assign = _restore_assignment_literal_to_head_simple_assignment(lit)
                if head_assign:
                    any_converted = True
                    condition = list(elem.condition)
                    # Create AssignmentAggregateElement
                    new_elem = AssignmentAggregateElement(
                        elem.location, head_assign, condition
                    )
                    new_elements.append(new_elem)
            else:
                # not a protected assignment element: keep it
                new_elements.append(elem)

        if not any_converted:
            return None

        return ChoiceAssignment(head.location, new_elements, head.left, head.right)

    def rewrite(self, node: StatementAST) -> FASP_Statement:
        """
        If node is an ast.StatementRule with protected assignment head(s), reconstruct an AssignmentRule.
        Otherwise return node unchanged.
        """
        if isinstance(node, ast.StatementRule):

            head = node.head
            body = list(node.body) if hasattr(node, "body") else []

            # CASE 1: HeadSimpleLiteral
            if isinstance(head, ast.HeadSimpleLiteral):
                # get literal attribute safely
                lit = head.literal
                if isinstance(
                    lit, ast.LiteralSymbolic
                ) and _literal_is_protected_assignment(lit):
                    head_assign = _restore_assignment_literal_to_head_simple_assignment(
                        lit
                    )
                    if head_assign is not None:
                        return AssignmentRule(node.location, head_assign, body)
                # otherwise unchanged
                return node

            # CASE 2: HeadSetAggregate (choice/aggregate protected assignment)
            elif isinstance(head, ast.HeadSetAggregate):
                choice_assignment = self._restore_set_aggregate_head(head)
                if choice_assignment is not None:
                    return AssignmentRule(node.location, choice_assignment, body)
                return node

            # CASE 3: HeadAggregate: This might occur after running clingo.rewrite.
            # Need to correct the restoration for this case.
            elif isinstance(head, ast.HeadAggregate):
                new_elements: Any = []
                any_converted = False
                for element in head.elements:
                    for term in element.tuple:
                        if (
                            isinstance(term, ast.TermFunction)
                            and term.name == ASSIGNMENT_NAME
                        ):
                            any_converted = True
                            new_element = (
                                _restore_assignment_term_function_to_head_simple_assignment(
                                    term
                                )
                                or term
                            )
                            new_elements.append(new_element)
                        else:
                            new_elements.append(term)

                    if isinstance(
                        element.literal, ast.LiteralSymbolic
                    ) and _literal_is_protected_assignment(element.literal):
                        any_converted = True
                        new_literal = (
                            _restore_assignment_literal_to_head_simple_assignment(
                                element.literal
                            )
                            or element.literal
                        )
                        new_elements.append(new_literal)
                    else:
                        new_elements.append(element.literal)

                    for condition in element.condition:
                        # if isinstance(
                        #     condition, ast.LiteralSymbolic
                        # ) and _literal_is_protected_assignment(condition):
                        #     any_converted = True
                        #     new_condition = (
                        #         _restore_assignment_literal_to_head_simple_assignment(
                        #             element.literal
                        #         )
                        #         or element.literal
                        #     )
                        #     new_elements.append(new_condition)
                        # else:
                        new_elements.append(condition)
                if not any_converted:
                    return node

                assert isinstance(new_literal, HeadSimpleAssignment)
                new_head = HeadAssignmentAggregate(
                    head.location,
                    new_literal.assigned_function,
                    head.function,
                    new_elements,
                )
                return AssignmentRule(node.location, new_head, body)

        # default
        return node


def restore_assignments(
    library: ELibrary, statements: Iterable[StatementAST]
) -> Iterable[FASP_Statement]:
    """
    Apply the restoration transformer to all statements.
    """
    transformer = _AssignmentRestorationTransformer(library)
    return (transformer.rewrite(statement) for statement in statements)


# #########################################################################
