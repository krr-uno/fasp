import re
import stat
from typing import NamedTuple

from click import STRING, group
from tomlkit import comment

_RE_LINE_COMMENT = r"%(?:$|[^*][^\n]*$)"
_PATTERN_BLOCK_COMMENT_DELIMITERS = re.compile("\n|%\*|\*%|%", re.MULTILINE)


def _find_all_block_comment_delimiters(source: str) -> list[re.Match]:
    """
    Find all block comment delimiters in the source code.
    """
    return _PATTERN_BLOCK_COMMENT_DELIMITERS.finditer(source)


_STATE_DEFAULT = 0
_STATE_LINE_COMMENT = 1
_STATE_BLOCK_COMMENT = 2


def _find_block_comment_delimiters(source: str) -> list[re.Match]:
    """
    Find all block comment delimiters in the source code.
    """
    delimiters = _PATTERN_BLOCK_COMMENT_DELIMITERS.finditer(source)
    states = [_STATE_DEFAULT]
    tokens = []
    for delim in delimiters:
        s = delim.group(0)
        if s == "%" and states[-1] != _STATE_BLOCK_COMMENT:
            tokens.append(delim)
            states.append(_STATE_LINE_COMMENT)
        elif s == "\n":
            tokens.append(delim)
            if states[-1] == _STATE_LINE_COMMENT:
                states.pop()
        elif s == "%*":
            if states[-1] != _STATE_LINE_COMMENT:
                if states[-1] == _STATE_DEFAULT:
                    tokens.append(delim)
                states.append(_STATE_BLOCK_COMMENT)
        elif s == "*%":
            if states[-1] != _STATE_LINE_COMMENT:
                states.pop()
            if states[-1] == _STATE_DEFAULT:
                tokens.append(delim)
    return tokens


l = list(
    _find_block_comment_delimiters(
        r"""\
    f(1) %* bbbbbbbbb := *% := 
     %    %*  
    #sum %* dddddddddd := % %* "="
                                     eeeeeeeeee
                                     fffffffff *%
                                     *% { x : p(X)} 
                                       % :=
                                    ."""
    )
)

l2 = [(d.group(0), d.span()) for d in l]
print(l2)

_PATTERN_ASSIGNMENT_TOKEN = re.compile(r":=", re.MULTILINE)


def find_assignment_tokens(source: str):
    """
    Find all occurrences of assignments in the source code.
    There are three types of assignments:
        f(t1) := t2
        f(t1) := #sum{ ... }
        f(t1) in { t2 : B }
    comments can appear before and after := or in.
    """
    comment_delimiters = _find_block_comment_delimiters(source)
    state = _STATE_DEFAULT
    tokens = []
    for delim in comment_delimiters:
        s = delim.group(0)
        if state == _STATE_DEFAULT:
            if s == "%*":
                state = _STATE_BLOCK_COMMENT
            elif s == "%":
                state = _STATE_LINE_COMMENT
            tokens.append(delim)
        elif state == _STATE_LINE_COMMENT and s == "\n":
            state = _STATE_DEFAULT
        elif state == _STATE_BLOCK_COMMENT and s == "*%":
            state = _STATE_DEFAULT

    return tokens


l = list(
    find_assignment_tokens(
        r"""\
    f(1) %* bbbbbbbbb := *% := 
     %    %*  
    #sum %* dddddddddd := % %* "="
                                     eeeeeeeeee
                                     fffffffff *%
                                     *% { x : p(X)} 
                                       % :=
                                    ."""
    )
)

l2 = [(d.group(0), d.span()) for d in l]
print(l2)

_PATTERN_ASSIGNMENT_TOKEN = re.compile(r":=", re.MULTILINE)


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


# class Token(NamedTuple):
#     type: int
#     value: str
#     line: int
#     column: int


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
