from platform import node
from typing import AbstractSet, Any, NamedTuple, Optional

from clingo import ast


class SyntacticError(NamedTuple):
    """
    Represents a syntactic error in the AST.

    Attributes:
        location: The location in the source code where the error occurred.
        message (str): The error message.
        information (any): Additional information about the error.
    """

    location: ast.Location
    message: str
    information: Optional[Any] = None

    def __str__(self) -> str:
        return f"{self.location}: error: syntax error, {self.message}"


class EnhancedTransformer(ast.Transformer):

    def visit_Rule(self, node: ast.AST, *args: Any, **kwargs: Any) -> ast.AST:
        """
        Visit the Rule node, setting the 'head' flag accordingly for children.

        Parameters
        ----------
        node : ast.AST
            The Rule node.

        Returns
        -------
        AST
            The (potentially transformed) Rule node.
        """
        kwargs["head"] = True
        head = self.visit(node.head, *args, **kwargs)
        kwargs["head"] = False
        body = self.visit_sequence(node.body, *args, **kwargs)
        if head is node.head and body is node.body:
            return node
        return node.update(head=head, body=body)

    def visit_Literal(self, node: ast.AST, *args, **kwargs: Any) -> ast.AST:
        """
        Visit a literal node and propagate its sign to children via kwargs.

        Parameters
        ----------
        node : ast.AST
            The literal node to visit.

        Returns
        -------
        AST
            The (potentially transformed) Literal node.
        """
        kwargs["sign"] = node.sign
        kwargs["location"] = node.location
        return node.update(**self.visit_children(node, *args, **kwargs))


class SyntacticCheckVisitor(EnhancedTransformer):
    """
    A visitor that checks for syntactic errors in the AST.

    This visitor does not perform any transformations but can be used to
    traverse the AST and check for specific syntactic conditions.
    """

    def __init__(self, invalid_ASTTypes: AbstractSet[ast.ASTType]) -> None:
        """
        Initializes the SyntacticCheckVisitor.

        Args:
            invalid_ASTTypes (set[ast.ASTType]): A set of AST types that are considered invalid.
        """
        super().__init__()
        self.invalid_ASTTypes = invalid_ASTTypes
        self.errors = []

    def visit(self, node: ast.AST, *args: Any, **kwargs: Any) -> ast.AST:
        """
        Visit the given AST node and check for invalid AST types.

        Parameters
        ----------
        node : ast.AST
            The AST node to visit.
        """
        if node.ast_type in self.invalid_ASTTypes:
            self.errors.append(
                SyntacticError(node.location, f"unexpected {node}", node.ast_type)
            )
            return node
        return super().visit(node, *args, **kwargs)


_POSITION = ast.Position("<aux>", 0, 0)
_LOCATION = ast.Location(_POSITION, _POSITION)


def create_literal(atom: ast.AST, sign: ast.Sign = ast.Sign.NoSign) -> ast.AST:
    if hasattr(atom, "location"):
        location = atom.location
    else:
        location = _LOCATION
    return ast.Literal(location, sign, ast.SymbolicAtom(atom))
