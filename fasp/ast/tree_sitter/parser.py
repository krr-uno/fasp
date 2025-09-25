# mypy: ignore-errors
# pragma: no cover
import ctypes
import importlib
import sys
from argparse import PARSER
from typing import Iterable, Optional, Sequence

from click import Path
from clingo import ast
from clingo.core import Location, Position
from tree_sitter import (
    Language,
    Node,
    Parser,
    Query,
    Tree,
)

from fasp.ast import (
    AssignmentRule,
    FASP_Statement,
    HeadAggregateAssignment,
    HeadChoiceAssignment,
    HeadSimpleAssignment,
)
from fasp.util.ast import (
    AST,
    ELibrary,
    ParsingException,
    SyntacticError,
    TermAST,
)
from fasp.util.ast import parse_string as clingo_parse_string

TREE_SITTER_LANG = "tree_sitter_fasp"


def _load_ts_language(
    module: Optional[str], so_path: Optional[Path] = None  # pragma: no cover
) -> Language:
    if module:
        m = importlib.import_module(module)  # e.g., tree_sitter_clingo
        cap = m.language()
        return cap if isinstance(cap, Language) else Language(cap)
    if so_path:
        lib = ctypes.CDLL(str(so_path))
        if not hasattr(lib, "TREE_SITTER_LANG"):
            raise RuntimeError(f"{so_path} does not export {TREE_SITTER_LANG}")
        lib.tree_sitter_fasp.restype = ctypes.c_void_p
        ptr = lib.tree_sitter_fasp()
        if not ptr:
            raise RuntimeError(f"{TREE_SITTER_LANG}() returned NULL")
        return Language(ptr)
    raise SystemExit(
        f"Provide --module {TREE_SITTER_LANG} OR --so ./parser.(so|dylib|dll)"
    )


def _ast_merge(asts1: Iterable[AST], asts2: Iterable[AST]) -> Iterable[AST]:
    asts = []
    asts1_i = iter(asts1)
    asts2_i = iter(asts2)
    ast1 = next(asts1_i, None)
    ast2 = next(asts2_i, None)
    while ast1 and ast2:
        if ast1.location < ast2.location:
            asts.append(ast1)
            ast1 = next(asts1_i, None)
        else:
            asts.append(ast2)
            ast2 = next(asts2_i, None)
    while ast1:
        asts.append(ast1)
        ast1 = next(asts1_i, None)
    while ast2:
        asts.append(ast2)
        ast2 = next(asts2_i, None)
    return asts


# def first_valid_leaf(
#     node,
#     skip_missing: bool = True,
#     skip_extra: bool = True,
# ) -> Optional[Node]:

#     def is_valid(n):
#         return (not skip_missing or not n.is_missing) and (
#             not skip_extra or not n.is_extra
#         )

#     if not node.children:
#         if is_valid(node):
#             return node
#         else:
#             return None
#     return first_valid_leaf(node.children[0])


# def first_sibling_valid_leaf(
#     node,
#     skip_missing: bool = True,
#     skip_extra: bool = True,
# ) -> Optional[Node]:

#     while node is not None:
#         leaf = first_valid_leaf(node, skip_missing, skip_extra)
#         if leaf:
#             return leaf
#         node = node.next_sibling
#     return None


# def next_valid_leaf(
#     node,
#     skip_missing: bool = True,
#     skip_extra: bool = True,
# ) -> Optional[Node]:
#     """
#     Return the next valid node in pre-order (document order),
#     skipping nodes that are missing or extra.
#     Returns None if `node` is the last valid node.
#     """
#     print("NEXT", node)
#     if node is None:
#         return None
#     leaf = first_sibling_valid_leaf(node, skip_missing, skip_extra)
#     if leaf:
#         return leaf
#     parent = node.parent
#     while parent is not None:
#         leaf = next_valid_leaf(parent.next_sibling, skip_missing, skip_extra)
#         if leaf:
#             return leaf
#         parent = parent.parent
#     return None


# def last_valid_leaf(
#     node,
#     skip_missing: bool = True,
#     skip_extra: bool = True,
# ) -> Optional[Node]:

#     def is_valid(n):
#         return (not skip_missing or not n.is_missing) and (
#             not skip_extra or not n.is_extra
#         )

#     if not node.children:
#         if is_valid(node):
#             return node
#         else:
#             return None
#     return last_valid_leaf(node.children[-1])


# def last_sibling_valid_leaf(
#     node,
#     skip_missing: bool = True,
#     skip_extra: bool = True,
# ) -> Optional[Node]:

#     while node is not None:
#         leaf = last_valid_leaf(node, skip_missing, skip_extra)
#         if leaf:
#             return leaf
#         node = node.previous_sibling
#     return None


# def is_valid_node(node: Node) -> bool | None:
#     if node.is_missing or node.is_error:
#         return False
#     if not node.children:
#         return None if node.is_extra else True
#     children_result = [d for c in node.children if (d := is_valid_node(c)) is not None]
#     if not children_result:
#         return None
#     return all(children_result)

# def previous_valid_sibling(node: Node) -> Optional[Node]:
#     node = node.prev_sibling
#     while node is not None:
#         if is_valid_node(node):
#             return node
#         node = node.prev_sibling
#     return None

# def previous_valid_node(
#     node: Node,
# ) -> Optional[Node]:
#     """
#     Return the next valid node in pre-order (document order),
#     skipping nodes that are missing or extra.
#     Returns None if `node` is the last valid node.
#     """
#     valid_node = previous_valid_sibling(node)
#     if valid_node is not None:
#         return valid_node
#     parent = node.parent
#     return previous_valid_node(parent) if parent is not None else None


LANGUAGE = _load_ts_language(TREE_SITTER_LANG)
PARSER = Parser(LANGUAGE)
QUERY_ERRORS = Query(LANGUAGE, "(ERROR) @error-node")
QUERY_MISSING = Query(LANGUAGE, "(MISSING) @missing-node")
QUERY_ASSIGMENT_RULE = Query(LANGUAGE, "(assignment_rule) @match")
QUERY_BODY_AGGREGATE_ELEMENTS = Query(LANGUAGE, "(body_aggregate_elements) @match")


class TreeSitterParser:
    """
    A simple wrapper around the Tree-sitter parser.
    """

    def __init__(self, library: ELibrary):
        self.library = library
        self.errors = []

    def parse(self, src: str) -> AST:
        """
        Parse source code into an AST.
        """
        src_bytes = bytes(src, "utf8")
        nodes = self._tree_parse_get_assignment_rules(src_bytes)
        assigment_rules = []
        parsing_errors = []
        for node in nodes:
            try:
                assigment_rules.append(self._parse_assignment_rule(node))
            except ParsingException as e:  # pragma: no cover
                parsing_errors.extend(e.errors)
        src_array = bytearray(src_bytes)
        for node in nodes:
            for i in range(node.start_byte, node.end_byte):
                src_array[i] = ord(b" ")
        src_bytes = bytes(src_array)
        src2 = src_bytes.decode("utf-8")
        statements = clingo_parse_string(self.library, src2)
        if parsing_errors:  # pragma: no cover
            raise ParsingException(parsing_errors)
        return _ast_merge(assigment_rules, statements[1:])

    def _parse_assignment_rule(self, node: Node) -> AST:
        self._check_errors(node)
        unparsed_head = node.child_by_field_name("head")
        if unparsed_head.type == "simple_assignment":
            head = self._parse_simple_assignment(unparsed_head)
        elif unparsed_head.type == "aggregate_assignment":
            head = self._parse_aggregate_assignment(unparsed_head)
        else:  # pragma: no cover
            head = self._parse_choice_assignment(unparsed_head)
        unparsed_body = node.child_by_field_name("body")
        if unparsed_body:
            body = self._clingo_parse_body(unparsed_body.text.decode("utf8"))
        else:
            body = []
        return AssignmentRule(
            self._location_from_node(node),
            head,
            body,
        )

    def _check_errors(self, node: Node) -> None:
        if node.has_error:
            start = Position(
                self.library.library,
                "<string>",
                node.start_point.row + 1,
                node.start_point.column + 1,
            )
            end = Position(
                self.library.library,
                "<string>",
                node.end_point.row + 1,
                node.end_point.column + 1,
            )
            location = Location(start, end)
            message = f"{node.text.decode('utf-8').replace('\n', ' ').strip()}"
            raise ParsingException([SyntacticError(location, message, None)])

    def _preparse_assignment(self, node: Node) -> tuple[TermAST, str]:
        unparsed_function = node.children[0].text.decode("utf-8")
        unparsed_value = "".join(
            map(lambda x: x.text.decode("utf-8"), node.children[2:])
        )
        assigned_function = ast.parse_term(self.library.library, unparsed_function)
        return assigned_function, unparsed_value

    def _parse_simple_assignment(self, node: Node) -> AST:
        assigned_function, unparsed_value = self._preparse_assignment(node)
        value = ast.parse_term(self.library.library, unparsed_value)
        return HeadSimpleAssignment(
            self._location_from_node(node),
            assigned_function,
            value,
        )

    def _parse_aggregate_assignment(self, node: Node) -> AST:
        assigned_function, unparsed_aggregate = self._preparse_assignment(node)
        aggregate = self._clingo_parse_body_aggregate(unparsed_aggregate)
        return HeadAggregateAssignment(
            self._location_from_node(node),
            assigned_function,
            aggregate.function,
            aggregate.elements,
        )

    def _parse_choice_assignment(self, node: Node) -> AST:  # pragma: no cover
        assigned_function, unparsed_choice = self._preparse_assignment(node)
        choice = self._clingo_parse_body_choice(unparsed_choice)
        return HeadChoiceAssignment(
            self.library.library,
            self._location_from_node(node),
            assigned_function,
            choice.elements,
        )

    def _location_from_node(self, node: Node) -> Location:
        return Location(
            Position(
                self.library.library,
                "<string>",
                node.start_point.row + 1,
                node.start_point.column + 1,
            ),
            Position(
                self.library.library,
                "<string>",
                node.end_point.row + 1,
                node.end_point.column + 1,
            ),
        )

    def _clingo_parse_body(self, src: str) -> AST:
        statement = ast.parse_statement(self.library.library, f":- {src}.")
        return statement.body

    def _clingo_parse_body_aggregate(self, src: str) -> AST:
        body = self._clingo_parse_body(src)
        return body[0]

    def _clingo_parse_body_choice(self, src: str) -> AST:  # pragma: no cover
        body = self._clingo_parse_body("#count" + src)
        return body[0]

    def _tree_parse(self, src: bytes) -> Tree:
        """
        Parse source code into a Tree-sitter parse tree.
        """
        return PARSER.parse(src)

    def _tree_parse_get_assignment_rules(self, src: bytes) -> list[Node]:
        """
        Return all assignment_rule nodes in the parse tree.
        """
        tree = self._tree_parse(src)
        return [
            node for node in tree.root_node.children if node.type == "assignment_rule"
        ]

        # self._check_syntax_errors(tree, src)
        # return TreeSitterParser._find_with_query(
        #     tree.root_node, QUERY_ASSIGMENT_RULE
        # )  # this is not deterministic

    # def _process_error(self, node: Node, is_missing: bool = False):
    #     pass
    # error = first_valid_leaf(node)
    # state = error.parse_state if error.is_named else error.next_parse_state
    # print("ERROR NODE", node, error, error.is_named)

    # error_after = f"around '{error.text.decode("utf-8")}'"
    # lookahead = set(self.language.lookahead_iterator(state).names())
    # lookahead.discard("line_comment")
    # lookahead.discard("block_comment")
    # print(set(self.language.lookahead_iterator(error.parse_state).names()))
    # print(set(self.language.lookahead_iterator(error.next_parse_state).names()))
    # if "." in lookahead and "," in lookahead:
    #     expected = ", expected '.' or ','"
    # elif "." in lookahead:
    #     expected = ", expected '.'"
    # elif ":-" in lookahead:
    #     expected = ", expected ':-'"
    # elif "term" in lookahead:
    #     expected = ", expected term"
    # elif "," in lookahead:
    #     expected = ", expected ','"
    # elif "}" in lookahead:
    #     expected = ", expected '}'"
    # elif "(" in lookahead:
    #     expected = ", expected '('"
    # elif ")" in lookahead:
    #     expected = ", expected ')'"
    # elif ":=" in lookahead:
    #     expected = ", expected ':='"

    # elif len(lookahead) == 1:
    #     expected = f", expected '{list(lookahead)[0]}'"
    # else:
    #     expected = ""
    # self.errors.append(
    #         f"<string>:{error.start_point[0]+1}:{error.start_point[1]+1}-{error.end_point[1]+1}: error {error_after}{expected}"
    #     )

    # def _check_syntax_errors(self, tree: Tree, src: bytes):
    #     # print(format_ts_tree(tree.root_node))
    #     errors = TreeSitterParser._find_with_query(
    #         tree, self.query_errors, "error-node"
    #     )
    #     # print("ERRORS", errors)
    #     for error in errors:
    #         self._process_error(error)
    #     # missing = TreeSitterParser._find_with_query(tree, self.query_missing, "missing-node")

    #     missing = []

    #     def traverse_tree(node: Node):
    #         for n in node.children:
    #             if n.is_missing:
    #                 missing.append(n)
    #             traverse_tree(n)

    #     traverse_tree(tree.root_node)

    #     # print("MISSING", missing)
    #     # for miss in missing:
    #     #     previous_node = previous_valid_leaf(
    #     #         miss, skip_missing=True, skip_extra=False
    #     #     )
    #     #     next_node = next_valid_leaf(miss, skip_missing=True, skip_extra=True)
    #     #     print("MISS", miss, previous_node, next_node)

    # @staticmethod
    # def _find_with_query(root: Node, query: Query, match: str = "match") -> list[Node]:
    #     """
    #     Return nodes of a given type using a simple query like '(<type>) @match'.
    #     """
    #     cursor = QueryCursor(query)
    #     results = []
    #     for capture_name, nodes in cursor.captures(root).items():
    #         # print(match, capture_name, nodes)
    #         if capture_name == match:
    #             results.extend(nodes)
    #     return results


# t = TreeSitterParser()
# tree = t.parse("a := 1 :- b.")
# print(tree.root_node)


def parse_string(library: ELibrary, src: str) -> Iterable[AST]:
    """
    Parse the programs in the given files and return an abstract syntax tree for
    each statement via a callback.

    The function follows clingo's handling of files on the command line. Filename
    `"-"` is treated as stdin and if an empty list is given, then the parser will
    read from stdin.

    Parameters
    ----------
    files
        List of file names.
    """
    parser = TreeSitterParser(library)
    asts = clingo_parse_string(library, "#program base.")
    asts.extend(parser.parse(src))
    return asts


def parse_files(
    library: ELibrary, files: Sequence[str] = None
) -> Iterable[FASP_Statement]:
    """
    Parse the programs in the given files and return an abstract syntax tree for
    each statement via a callback.

    The function follows clingo's handling of files on the command line. Filename
    `"-"` is treated as stdin and if an empty list is given, then the parser will
    read from stdin.

    Parameters
    ----------
    files
        List of file names.
    """
    if not files:
        files = ["-"]  # pragma: no cover
    parser = TreeSitterParser(library)
    asts = []
    for file in files:
        if file == "-":
            src = sys.stdin.read()  # pragma: no cover
        else:
            with open(file, "r", encoding="utf-8") as f:
                src = f.read()
        asts.extend(parser.parse(src))
    return asts
