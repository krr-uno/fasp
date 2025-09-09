from ast import unparse
import ctypes
import importlib

import stat
from typing import Iterable, Optional, Sequence
from attr import dataclass
from click import Path
from tree_sitter import Language, Node, Query, QueryCursor, Tree, Parser

from clingo import ast
from clingo.core import Library, Location, Position

from fasp.ast import AssignmentRule, HeadAggregateAssignment, HeadChoiceAssignment, HeadSimpleAssignment
from fasp.util.ast import AST, TermAST

def _load_ts_language(module: Optional[str], so_path: Optional[Path] = None) -> Language:
    if module:
        m = importlib.import_module(module)  # e.g., tree_sitter_clingo
        cap = m.language()
        return cap if isinstance(cap, Language) else Language(cap)
    if so_path:
        lib = ctypes.CDLL(str(so_path))
        if not hasattr(lib, "tree_sitter_clingo"):
            raise RuntimeError(f"{so_path} does not export tree_sitter_clingo")
        lib.tree_sitter_clingo.restype = ctypes.c_void_p
        ptr = lib.tree_sitter_clingo()
        if not ptr:
            raise RuntimeError("tree_sitter_clingo() returned NULL")
        return Language(ptr)
    raise SystemExit("Provide --module tree_sitter_clingo OR --so ./parser.(so|dylib|dll)")



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


class TreeSitterParser:
    """
    A simple wrapper around the Tree-sitter parser.
    """

    def __init__(self, library: Library):
        self.library = library
        self.language = _load_ts_language("tree_sitter_clingo")
        self.parser = Parser(self.language)
        self.assignment_rule_query = Query(self.language, "(assignment_rule) @match")

    def parse(self, src: str) -> AST:
        """
        Parse source code into an AST.
        """
        src_bytes = bytes(src, "utf8")
        nodes = self._tree_parse_assignments(src_bytes)
        assigment_rules = [self._parse_assignment_rule(n) for n in nodes]
        src_array = bytearray(src_bytes)
        for node in nodes:
            for i in range(node.start_byte, node.end_byte):
                src_array[i] = ord(b" ")
        src_bytes = bytes(src_array)
        src2 = src_bytes.decode("utf-8")
        statements = []
        ast.parse_string(self.library, src2, statements.append)
        return _ast_merge(assigment_rules, statements[1:])


    def _parse_assignment_rule(self, node: Node) -> AST:
        children = node.children
        unparsed_head = children[0]
        if unparsed_head.type == "simple_assignment":
            head = self._parse_simple_assignment(unparsed_head)
        elif unparsed_head.type == "aggregate_assignment":
            head = self._parse_aggregate_assignment(unparsed_head)
        else:
            head = self._parse_choice_assignment(unparsed_head)
        if len(children) > 2:
            unparse_body = children[2]
            body = self._clingo_parse_body(unparse_body.text.decode("utf8"))
        else:
            body = []

        return AssignmentRule(
            self.library,
            self._location_from_node(node),
            head,
            body,
        )
    
    def _preparse_assignment(self, node: Node) -> tuple[TermAST, str]:
        unparsed_function = node.children[0].text.decode("utf-8")
        unparsed_value = node.children[2].text.decode("utf-8")
        assigned_function = ast.parse_term(self.library, unparsed_function)
        return assigned_function, unparsed_value

    def _parse_simple_assignment(self, node: Node) -> AST:
        assigned_function, unparsed_value = self._preparse_assignment(node)
        value = ast.parse_term(self.library, unparsed_value)
        return HeadSimpleAssignment(
            self.library,
            self._location_from_node(node),
            assigned_function,
            value,
        )
    
    def _parse_aggregate_assignment(self, node: Node) -> AST:
        assigned_function, unparsed_aggregate = self._preparse_assignment(node)
        aggregate = self._clingo_parse_body_aggregate(unparsed_aggregate)
        return HeadAggregateAssignment(
            self.library,
            self._location_from_node(node),
            assigned_function,
            aggregate.function,
            aggregate.elements,
        )

    def _parse_choice_assignment(self, node: Node) -> AST:
        assigned_function, unparsed_choice = self._preparse_assignment(node)
        choice = self._clingo_parse_body_choice(unparsed_choice)
        return HeadChoiceAssignment(
            self.library,
            self._location_from_node(node),
            assigned_function,
            choice.elements,
        )
    

    def _location_from_node(self, node: Node) -> Location:
        return Location(
            Position(
                self.library,
                "<string>",
                node.start_point.row+1,
                node.start_point.column+1,
                ),
            Position(
                self.library,
                "<string>",
                node.end_point.row+1,
                node.end_point.column+1,
                ),
        )

    def _clingo_parse_body(self, src: str) -> AST:
        statement = ast.parse_statement(self.library, f":- {src}")
        return statement.body

    def _clingo_parse_body_aggregate(self, src: str) -> AST:
        body = self._clingo_parse_body(src + ".")
        return body[0]

    def _clingo_parse_body_choice(self, src: str) -> AST:
        body = self._clingo_parse_body("#count" + src + ".")
        return body[0]

    def _tree_parse(self, src: bytes) -> Tree:
        """
        Parse source code into a Tree-sitter parse tree.
        """
        return self.parser.parse(src)

    def _tree_parse_assignments(self, src: bytes) -> list[Node]:
        """
        Return all assignment_rule nodes in the parse tree.
        """
        root = self._tree_parse(src)
        return TreeSitterParser._find_with_query(root, self.assignment_rule_query)

    @staticmethod
    def _find_with_query(root: Node, query: Query):
        """
        Return nodes of a given type using a simple query like '(<type>) @match'.
        """
        cursor = QueryCursor(query)

        results = []
        for capture_name, nodes in cursor.captures(root.root_node).items():
            if capture_name == "match":
                results.extend(nodes)
        return results

    
# t = TreeSitterParser()
# tree = t.parse("a := 1 :- b.")
# print(tree.root_node)