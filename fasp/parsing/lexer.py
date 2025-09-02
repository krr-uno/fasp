import regex as re

# import re
from tkinter import N, NO
from tracemalloc import start
from turtle import st
from typing import NamedTuple, Sequence, Sequence

from click import STRING, group
from regex import E
from tomlkit import comment

_RE_UNESCAPED_CHAR = r'[^"\\]'
_RE_ESCAPE_SEQUENCE = (
    r"\\[^xu0-7]|[0-7]{1,3}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|u\{[0-9a-fA-F]+\}"
)
_RE_STRING = rf'"({_RE_UNESCAPED_CHAR}|{_RE_ESCAPE_SEQUENCE})*"?'


_SPECIFICATION_NON_CODE_BLOCKS_DEFAULT = [
    ("STRING", _RE_STRING),  # String
    ("B_COMMENT_START", r"%\*"),  # Block comment start
    ("B_COMMENT_END", r"\*%"),  # Block comment end
    ("L_COMMENT_START", r"%"),  # Line comment start
    ("ASSIGN", r":="),  # Assignment operator
]

_PATTERN_NON_CODE_BLOCKS_DEFAULT = re.compile(
    "|".join(
        f"(?P<{name}>{pattern})"
        for name, pattern in _SPECIFICATION_NON_CODE_BLOCKS_DEFAULT
    ),
    re.MULTILINE,
)

_PATTERN_DOT = re.compile(r"(?<!\.)\.(?!\.)")
_PATTERN_DOT_REVERSE = re.compile(r"(?<!\.)\.(?!\.)", re.REVERSE)


class NonCodeToken(NamedTuple):
    type: str
    start: int
    end: int


class AssignmentToken(NamedTuple):
    type: str | None
    start: int
    end: int
    previous_non_code_tokens: list[NonCodeToken]


class ErrorToken(NamedTuple):
    type: str
    start: int
    end: int


PreToken = AssignmentToken | ErrorToken


def _find_assignments(source: str) -> list[PreToken]:
    """
    Find all non-code blocks (line comments and block comments) in the source code.
    """
    no_code_tokens = []
    assignment_tokens = []
    stack = [None]
    match = _PATTERN_NON_CODE_BLOCKS_DEFAULT.search(source)
    while match:
        kind = match.lastgroup
        value = match.group()
        if stack[-1] is None:
            if kind == "ASSIGN":
                assignment_tokens.append(
                    AssignmentToken(kind, match.start(), match.end(), no_code_tokens)
                )
                no_code_tokens = []
            elif kind == "STRING":
                if len(value) == 1 or value[-1] != '"':
                    kind = "UNCLOSED_STRING"
                    end = len(source)
                else:
                    end = match.end()
                no_code_tokens.append(NonCodeToken(kind, match.start(), end))
            elif kind == "L_COMMENT_START":
                end = source.find("\n", match.start() + 1)
                end = end + 1 if end != -1 else len(source)
                no_code_tokens.append(NonCodeToken("L_COMMENT", match.start(), end))
            else:  #  kind =="B_COMMENT_START"}
                stack.append((kind, match.start()))
        else:  # stack[-1][0] == "B_COMMENT_START":
            if kind == "B_COMMENT_START":
                stack.append((kind, match.start()))
            elif kind == "B_COMMENT_END":
                open = stack.pop()
                if stack[-1] is None:
                    no_code_tokens.append(
                        NonCodeToken("B_COMMENT", open[1], match.end())
                    )
            pass
        match = _PATTERN_NON_CODE_BLOCKS_DEFAULT.search(source, match.end())
    #     print('"' * 30)
    #     print(stack)
    # print(1, no_code_tokens)
    if no_code_tokens:
        if no_code_tokens[-1].type != "UNCLOSED_STRING" and (
            assignment_tokens or len(stack) > 1
        ):
            assignment_tokens.append(
                AssignmentToken(None, len(source), len(source), no_code_tokens)
            )
        # print(2, no_code_tokens)
        if no_code_tokens[-1].type == "UNCLOSED_STRING":
            if len(no_code_tokens) > 1:
                pos = no_code_tokens[-1].start
                assignment_tokens.append(
                    AssignmentToken(None, pos, pos, no_code_tokens[:-1])
                )
            assignment_tokens.append(
                ErrorToken("UNCLOSED_STRING", no_code_tokens[-1].start, len(source))
            )
    if len(stack) > 1:
        open = stack[1]
        if open[0] == "B_COMMENT_START":
            assignment_tokens.append(
                ErrorToken("UNCLOSED_BLOCK_COMMENT", open[1], len(source))
            )
        else:  # open[0] == "UNCLOSED_STRING":
            assignment_tokens.append(
                ErrorToken("UNCLOSED_STRING", open[1], len(source))
            )
    # from pprint import pprint
    # print('"'*30)
    # for t in no_code_tokens:
    #     print(f"{t.type:10}", "\t", t.start, "\t",t.end, "\t",source[t.start:t.end])
    # print('"'*30)
    # for t in assignment_tokens:
    #     print(f"{str(t.type):10}", "\t", t.start, "\t",t.end, "\t",source[t.start:t.end])
    #     for t1 in t.previous_non_code_tokens:
    #         print(f"    {str(t1.type):10}", "\t", t1.start, "\t",t1.end, "\t",source[t1.start:t1.end])
    # print('"'*30)
    return assignment_tokens


class UnParsedClingoCode(NamedTuple):
    source: str
    start_line: int
    start_col: int


class UnParsedAssignmentRule(NamedTuple):
    source: str
    start_line: int
    start_col: int
    assignment_pos : int


def _find_previous_dot(
    source: str, start: int, end: int, non_code_tokens: Sequence[NonCodeToken]
):
    for token in reversed(non_code_tokens):
        match = _PATTERN_DOT_REVERSE.search(source, token.end, end)
        if match:
            return match
        end = token.start
    return _PATTERN_DOT_REVERSE.search(source, start, end)

def _find_next_dot(
    source: str, start: int, end: int, non_code_tokens: Sequence[NonCodeToken]
):
    for token in non_code_tokens:
        match = _PATTERN_DOT.search(source, start, token.start)
        if match:
            return match
        start = token.end
    return _PATTERN_DOT.search(source, start, end)


def split_code(source: str, tokens: list[PreToken]):
    assert tokens
    blocks = []
    start = 0
    token = tokens[0]
    start_line = 0
    if isinstance(token, AssignmentToken):
        next_pos = 1
        while True:
            if next_pos < len(tokens):
                end = tokens[next_pos].start
                next_non_code_tokens = tokens[next_pos].previous_non_code_tokens
            else:
                end = len(source)
                next_non_code_tokens = []
            rule_start = _find_previous_dot(source, start, token.start, token.previous_non_code_tokens)
            # print(start, token.start, token.previous_non_code_tokens, f"'{source[start:token.start]}'", rule_start)
            rule_start = start if rule_start is None else rule_start.end()
            rule_end = _find_next_dot(source, token.end, end, next_non_code_tokens)
            if rule_end is None:
                # expect . but not found
                pass
            rule_end = rule_end.end()
            if start < rule_start:
                last_line_break = source.rfind("\n", 0, start)
                if last_line_break == -1:
                    column = 0
                else:
                    column = start - last_line_break
                blocks.append(
                    UnParsedClingoCode(
                        source[start : rule_start],
                        start_line,
                        column,
                    )
                )
                start_line += source.count("\n", start, rule_start)
            last_line_break = source.rfind("\n", 0, rule_start)
            # print(f"{rule_start=}")
            if last_line_break == -1:
                column = rule_start
            else:
                column = rule_start - last_line_break - 1
            blocks.append(
                UnParsedAssignmentRule(
                    source[rule_start : rule_end],
                    start_line,
                    column,
                    token.start - rule_start,
                )
            )
            if next_pos >= len(tokens):
                break
            token = tokens[next_pos]
            start = rule_end
            next_pos += 1
            start_line += source.count("\n", rule_start, rule_end)
    # deal with error tokens
    if source[rule_end + 1:].strip():
        last_line_break = source.rfind("\n", 0, rule_end)
        if last_line_break == -1:
            column = rule_end
        else:
            column = rule_end - last_line_break - 1
        blocks.append(
            UnParsedClingoCode(
                source[rule_end:],
                start_line,
                column,
            )
        )
    return blocks


# _find_assignments(
#     r"""\
# p("this string has a %* %  *%. ..", %* block comment  and %* nested block comment % *% . .. *% :- body. % line comment
# %another line comment
# f("string.",4) %* block comment *% := %* block comment *% ("another string.",3) .
# % line comment"""
# )

# def _find_all_block_comment_delimiters(source: str) -> list[re.Match]:
#     """
#     Find all block comment delimiters in the source code.
#     """
#     return _PATTERN_BLOCK_COMMENT_DELIMITERS.finditer(source)

# _STATE_DEFAULT = 0
# _STATE_LINE_COMMENT = 1
# _STATE_BLOCK_COMMENT = 2

# def _find_block_comment_delimiters(source: str) -> list[re.Match]:
#     """
#     Find all block comment delimiters in the source code.
#     """
#     delimiters = _PATTERN_BLOCK_COMMENT_DELIMITERS.finditer(source)
#     states = [_STATE_DEFAULT]
#     tokens = []
#     for delim in delimiters:
#         s = delim.group(0)
#         if s == "%" and states[-1] != _STATE_BLOCK_COMMENT:
#             tokens.append(delim)
#             states.append(_STATE_LINE_COMMENT)
#         elif s == "\n":
#             tokens.append(delim)
#             if states[-1] == _STATE_LINE_COMMENT:
#                 states.pop()
#         elif s == "%*":
#             if states[-1] != _STATE_LINE_COMMENT:
#                 if states[-1] == _STATE_DEFAULT:
#                     tokens.append(delim)
#                 states.append(_STATE_BLOCK_COMMENT)
#         elif s == "*%":
#             if states[-1] != _STATE_LINE_COMMENT:
#                 states.pop()
#             if states[-1] == _STATE_DEFAULT:
#                 tokens.append(delim)
#     return tokens

# l = list(_find_block_comment_delimiters(r"""\
#     f(1) %* bbbbbbbbb := *% :=
#      %    %*
#     #sum %* dddddddddd := % %* "="
#                                      eeeeeeeeee
#                                      fffffffff *%
#                                      *% { x : p(X)}
#                                        % :=
#                                     ."""))

# l2 = [(d.group(0), d.span()) for d in l]
# print(l2)

# _PATTERN_ASSIGNMENT_TOKEN = re.compile(r":=", re.MULTILINE)


# def find_assignment_tokens(source: str):
#     """
#     Find all occurrences of assignments in the source code.
#     There are three types of assignments:
#         f(t1) := t2
#         f(t1) := #sum{ ... }
#         f(t1) in { t2 : B }
#     comments can appear before and after := or in.
#     """
#     comment_delimiters = _find_block_comment_delimiters(source)
#     state = _STATE_DEFAULT
#     tokens = []
#     for delim in comment_delimiters:
#         s = delim.group(0)
#         if state == _STATE_DEFAULT:
#             if s == "%*":
#                 state = _STATE_BLOCK_COMMENT
#             elif s == "%":
#                 state = _STATE_LINE_COMMENT
#             tokens.append(delim)
#         elif state == _STATE_LINE_COMMENT and s == "\n":
#             state = _STATE_DEFAULT
#         elif state == _STATE_BLOCK_COMMENT and s == "*%":
#             state = _STATE_DEFAULT

#     return tokens


# l = list(find_assignment_tokens(r"""\
#     f(1) %* bbbbbbbbb := *% :=
#      %    %*
#     #sum %* dddddddddd := % %* "="
#                                      eeeeeeeeee
#                                      fffffffff *%
#                                      *% { x : p(X)}
#                                        % :=
#                                     ."""))

# l2 = [(d.group(0), d.span()) for d in l]
# print(l2)

# _PATTERN_ASSIGNMENT_TOKEN = re.compile(r":=", re.MULTILINE)


# _RE_BLOCK_COMMENT = r"%\*[^*]*\*+(?:[^%*][^*]*\**+)*%"

# _PATTERN_LINE_COMMENT = re.compile(_RE_LINE_COMMENT, re.MULTILINE)
# _PATTERN_BLOCK_COMMENT = re.compile(_RE_BLOCK_COMMENT, re.MULTILINE)

# print(_PATTERN_LINE_COMMENT.findall(r"""\
#     aaaaa % bbbbbbbbb
#      %
#     ccccccccc % dddddddddd eeeeeeeeee"""))
# print(_PATTERN_BLOCK_COMMENT)


# class TokenType:
#     OTHER = 0
#     INTERVAL = 1
#     DOT = 2
#     RULEOP = 3
#     NEWLINE = 4
#     ASSIGN = 5
#     LBRACER = 6
#     RBRACER = 7
#     HASHTAG = 8
#     STRING = 9
#     UNCLOSED_STRING = 10

#     TOKEN_SPECIFICATION = [
#         ("INTERVAL", r".."),  # Interval operator
#         ("DOT", r"."),  # Statement terminator
#         ("RULEOP", r":-"),  # Statement terminator
#         ("NEWLINE", "\n"),  # Line endings
#         ("ASSIGN", r":="),  # Assignment operator
#         ("LBRACER", r"{"),
#         ("RBRACER", r"}"),
#         ("HASHTAG", r"#"),
#         ("STRING", STRING),  # String literal
#         ("UNCLOSED_STRING", r'"'),
#     ]


# UNESCAPED_STRING = r'[^"\\]+'
# ESCAPE_SEQUENCE = (
#     r"\\(?:[^xu0-7]|[0-7]{1,3}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|u\{[0-9a-fA-F]+\})"
# )
# STRING = rf'"(?:{UNESCAPED_STRING}|{ESCAPE_SEQUENCE})*"'


# TOKEN_MAP = {name: i for i, (name, _) in enumerate(TOKEN_SPECIFICATION)}

# TOKEN_SPECIFICATION = [
#     (n, re.escape(r) if n in {"STRING", "NEWLINE"} else r)
#     for n, r in TOKEN_SPECIFICATION
# ]

# TOKEN_REGEX = "|".join("(?P<%s>%s)" % pair for pair in TOKEN_SPECIFICATION)

# TOKEN_PATTERN = re.compile(TOKEN_REGEX)


# def tokenize(program: str):
#     line_num = 1
#     line_start = 0
#     for match in TOKEN_PATTERN.finditer(program):
#         kind = match.lastgroup
#         value = match.group()
#         column = match.start() - line_start
#         if kind == "NEWLINE":
#             line_start = match.end()
#             line_num += 1
#         yield Token(kind, value, line_num, column)


# for match in tokenize(
#     """\
# a :- b.
# c d..
# """
# ):
#     print(match)
# print("=" * 10)
# for match in tokenize('"a".'):
#     print(match)
# print("="*10)
# for match in tokenize('p("String", "Another string").'):
#     print(match)
# print("="*10)
# for match in tokenize('p("String, "Another string").'):
#     print(match)
# print("="*10)
