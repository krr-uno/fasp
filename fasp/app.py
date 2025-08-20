from typing import Sequence
import sys

from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.core import Library
from clingo.control import Control as ClingoControl

from fasp.ast.syntax_checking import ParsingException
from fasp.control import Control
from fasp.__version__ import __version__


class FaspApp(App):
    def __init__(self, library: Library, clingo_options: Sequence[str]) -> None:
        super().__init__("fasp", __version__)
        self._order = Flag()
        self._library = library
        self._clingo_options = clingo_options

    def register_options(self, options: AppOptions) -> None:
        options.add_flag(
            "fasp", "order", "Print atoms in models in order.", self._order
        )

    def main(self, clingo_control: ClingoControl, files: Sequence[str]) -> None:
        prefix = "F"
        control = Control(
            self._library,
            self._clingo_options,
            prefix,
            clingo_control,
        )
        try:
            control.parse_files(files)
        except ParsingException as e:
            for error in e.errors:
                sys.stderr.write(str(error) + "\n")
            sys.stderr.write(f"*** ERROR: (fasp): parsing failed")
            return
        control.main()


def fasp_main(
    library: Library, options: list[str] = [], raise_errors: bool = False
) -> int:
    """
    Main function for the fasp application.

    Parameters
    ----------
    library
        The Clingo library to use.
    options
        Command line options to pass to the application.
    raise_errors
        If True, raise exceptions on errors instead of printing them.
    """
    app = FaspApp(library, options)
    options.append("--outf=3")
    return clingo_main(library, options, app, raise_errors)


def main(options: Sequence[str] = []) -> int:
    with Library() as library:
        return fasp_main(library, list(options))
    return 1
