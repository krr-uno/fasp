from typing import Optional, Union

from tree_sitter import Node, Tree


def format_ts_tree(
    obj: Union[Tree, Node],
    source: Optional[Union[bytes, str]] = None,
    *,
    named_only: bool = False,
    show_fields: bool = True,
    snippet_mode: str = "leaves",  # "none" | "tokens" | "leaves" | "all"
    show_positions: bool = False,
    max_snippet: int = 40,
) -> str:
    """
    Pretty-print a Tree-Sitter tree (or node).

    snippet_mode:
      - "none":   never show snippets
      - "tokens": show only for unnamed nodes (punctuation, etc.)
      - "leaves": show for nodes with no children (default)
      - "all":    show for every node
    """
    # normalize source bytes
    if isinstance(source, str):
        src_bytes = source.encode("utf-8", "replace")
    else:
        src_bytes = source  # bytes or None

    # pick the root node
    node: Node = obj.root_node if hasattr(obj, "root_node") else obj  # type: ignore

    def get_children(n: Node) -> list[Node]:
        if not named_only:
            return list(n.children)
        # keep only named children in order
        return [c for c in n.children if c.is_named]

    def field_name(parent: Node, idx: int) -> Optional[str]:
        try:
            return parent.field_name_for_child(idx)  # type: ignore[attr-defined]
        except AttributeError:
            return None

    def should_snippet(n: Node) -> bool:
        if src_bytes is None:
            return False
        if snippet_mode == "none":
            return False
        if snippet_mode == "tokens":
            return not n.is_named
        if snippet_mode == "leaves":
            # leaves in the chosen view (respect named_only)
            return len(get_children(n)) == 0
        if snippet_mode == "all":
            return True
        return False

    def snippet(n: Node) -> str:
        if not should_snippet(n):
            return ""
        s = (
            src_bytes[n.start_byte : n.end_byte].decode("utf-8", "replace")
            if src_bytes
            else ""
        )
        s = s.replace("\n", "\\n")
        if len(s) > max_snippet:
            s = s[: max_snippet - 1] + "…"
        # quote if empty or contains whitespace
        if s == "" or any(ch.isspace() for ch in s):
            s = f'"{s}"'
        return s

    def label(n: Node) -> str:
        parts = [n.type]
        if show_positions:
            (sr, sc), (er, ec) = n.start_point, n.end_point
            parts.append(f"[{sr+1}:{sc+1}-{er+1}:{ec+1}]")
        sn = snippet(n)
        if sn:
            parts.append(f"= {sn}")
        return " ".join(parts)

    lines: list[str] = []

    def walk(
        n: Node, prefix: str = "", is_last: bool = True, fld: Optional[str] = None
    ):
        # branch glyphs
        branch = "└── " if is_last else "├── "
        # inline field name (if any)
        head = f"{fld}: " if (show_fields and fld) else ""
        lines.append(prefix + branch + head + label(n))

        kids_all = list(n.children)
        kids = []
        for i, ch in enumerate(kids_all):
            if named_only and not ch.is_named:
                continue
            kids.append((ch, field_name(n, i)))

        if not kids:
            return

        child_prefix = prefix + ("    " if is_last else "│   ")
        for j, (ch, ch_fld) in enumerate(kids):
            walk(ch, child_prefix, j == len(kids) - 1, ch_fld)

    walk(node)
    return "\n".join(lines)
