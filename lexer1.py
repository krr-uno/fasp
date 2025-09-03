# import re
# from typing import NamedTuple

# from click import STRING


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
# # for match in tokenize('"a".'):
# #     print(match)
# # print("="*10)
# # for match in tokenize('p("String", "Another string").'):
# #     print(match)
# # print("="*10)
# # for match in tokenize('p("String, "Another string").'):
# #     print(match)
# # print("="*10)
