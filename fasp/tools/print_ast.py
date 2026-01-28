"""Small script to parse ASP source using fasp's tree-sitter parser and print AST structure.

Usage: run from repository root:
    python -m fasp.tools.print_ast

It will import the `parse_string` function from `fasp.ast.parsing` which uses the TreeSitter parser
and merges its assignment ASTs with clingo ASTs. The script implements a generic visitor that
handles both fasp AssignmentAST and clingo AST nodes.
"""

import sys
from typing import Any

# IMPORTANT: use the parsing.parse_string that uses TreeSitter (supports ':=')
from fasp.syntax_tree.parsing.parser import parse_string
from fasp.util.ast import ELibrary


def pretty_print(node: Any, indent: int = 0) -> None:
    """Recursively print AST node structure.

    Supports fasp AssignmentAST types (they implement .to_dict / .visit) and
    clingo AST nodes which implement visit(callback) requiring a callback of the
    form callback(node).
    """
    prefix = "  " * indent
    if node is None:
        print(f"{prefix}<None>")
        return

    # Simple scalar types
    if isinstance(node, (str, int, float, bool)):
        print(f"{prefix}{repr(node)} ({type(node).__name__})")
        return

    # Sequence types
    try:
        # exclude strings
        if isinstance(node, (list, tuple)):
            print(f"{prefix}{type(node).__name__} [")
            for item in node:
                pretty_print(item, indent + 1)
            print(f"{prefix}]")
            return
    except Exception:
        pass

    # fasp custom AssignmentAST objects have a to_dict method
    if hasattr(node, "to_dict"):
        d = node.to_dict()
        t = d.get("type", type(node).__name__)
        print(f"{prefix}{t}: {str(node)}")
        for k, v in d.items():
            if k in {"type", "location"}:
                continue
            print(f"{prefix}  {k}:")
            pretty_print(v, indent + 2)
        return

    # clingo AST nodes: show type and string, then use node.visit to traverse children
    if hasattr(node, "visit") and hasattr(node, "__class__"):
        cls = node.__class__.__name__
        try:
            s = str(node)
        except Exception:
            s = "<unprintable>"
        print(f"{prefix}{cls}: {s}")

        # clingo's visit expects a callback taking one parameter: the child node
        def cb(child: Any) -> None:
            pretty_print(child, indent + 1)

        try:
            # Some clingo AST nodes expect visitor to be called as node.visit(callback)
            node.visit(cb)
        except TypeError:
            # clingo sometimes expects (visitor, *args) signature; try calling with visitor only
            try:
                node.visit(cb, None)
            except Exception:
                # Give up on visiting children
                pass
        return

    # Fallback: print repr
    print(f"{prefix}{type(node).__name__}: {repr(node)}")


if __name__ == "__main__":
    lib = ELibrary()
    if len(sys.argv) > 1:
        src = "\n".join(sys.argv[1:])
    else:
        # sample program with assignment and aggregate
        # src = """
        # father(cain):=adam.
        # n_orphan := #count{X : orphan(X)}.
        # a := 1 :- b.
        # b(X) := a+X :- b21(X); b22(X).
        # p(X) :- q(X).
        # t(1).
        # """
        src = "1 { f(X) := 1 : p(X); f(X) := 2 : q(X) } 3."
        src = "a :- p(C); #false: country(f(b))."
    print("Source:\n", src)
    asts = parse_string(lib, src)
    print("\n--- Parsed ASTs ---\n")
    for a in asts:
        pretty_print(a)
        print()

# a := #some{X: p(X)} :- Body.

# {a := X: p(X)} = 1 :- #count{X: p(X)} >=  1, Body
