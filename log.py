from clingo.core import Library
from clingo.ast import parse_string
statements = []
messages = []
library = Library(logger=lambda t, msg: messages.append((t, msg)))
try:
    parse_string(library, ":- #sum { X: p(X): q(X) }.", statements.append)
except Exception as e:
    print(messages)