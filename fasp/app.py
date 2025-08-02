from typing import Sequence
import sys

from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.core import Library
from clingo.control import Control as ClingoControl

from fasp.control import Control

class FaspApp(App):
    def __init__(self, library: Library, clingo_options: Sequence[str]) -> None:
        super().__init__("fasp", "1.0.0")
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
        control.parse_files(files)
        control.main()


def fasp_main(
    library: Library, options: Sequence[str] = [], raise_errors: bool = False
) -> None:
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
    clingo_main(library, options, app, raise_errors)


def main():
    with Library() as library:
        fasp_main(library, sys.argv[1:])
