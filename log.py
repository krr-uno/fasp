from clingo.core import Library
from clingo.ast import parse_string
from clingo.app import App, clingo_main

statements = []
messages = []
try:
    with Library(logger=lambda t, msg: messages.append((t, msg))) as library:
            parse_string(library, ":- #sum { X: p(X): q(X) }.", statements.append)
except Exception as e:
    print("MSG1", messages)
    # print(e)


class FaspApp(App):
     def main(self, clingo_control, files) -> None:
        return None

messages = []
# try:
with Library(logger=lambda t, msg: messages.append((t, msg))) as library2:
    clingo_main(library2, [], FaspApp())
# except Exception as e:
print("MSG2", messages)
    # raise e