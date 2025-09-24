import sys
from typing import Sequence

from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.control import Control as ClingoControl
from clingo.core import Library

from fasp.__version__ import __version__
from fasp.ast.rewriting.collectors import ParsingException
from fasp.control import Control


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
        except ParsingException as e:  # pragma: no cover
            for error in e.errors:
                sys.stderr.write(str(error) + "\n")
            sys.stderr.write("*** ERROR: (fasp): parsing failed")
            return
        control.main()


def fasp_main(
    library: Library, options: list[str] | None = None, raise_errors: bool = False
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
    if options is None:  # pragma: no cover
        options = []
    app = FaspApp(library, options)
    options.append("--outf=3")
    try:
        return clingo_main(library, options, app)
    except Exception:  # pragma: no cover
        if raise_errors:
            raise
        return 1


def main(options: Sequence[str] = ()) -> int:
    with Library() as library:
        return fasp_main(library, list(options))
    return 1  # pragma: no cover
