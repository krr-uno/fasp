import sys
from typing import Sequence

from click import argument
from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.control import Control as ClingoControl

from fasp.__version__ import __version__
from fasp.control import Control
from fasp.util.ast import ELibrary, ParsingException


class FaspApp(App):
    def __init__(self, library: ELibrary, clingo_options: Sequence[str]) -> None:
        super().__init__("fasp", __version__)
        self._order = Flag()
        self._library = library
        self._clingo_options = clingo_options
        self._prefix = "F"
        self._print_rewrite = False
        self._check_command_line_arguments(clingo_options)

    def _check_command_line_arguments(self, arguments: Sequence[str]) -> None:
        for i, arg in enumerate(arguments):
            if arg.startswith("--mode="):
                if arg == "--mode=rewrite":
                    self._print_rewrite = True
                elif len(arguments) > i + 1 and arguments[i + 1] == "rewrite":
                    self._print_rewrite = True

    def register_options(self, options: AppOptions) -> None:
        options.add_flag(
            "fasp", "order", "Print atoms in models in order.", self._order
        )

        options.add(
            "fasp",
            "prefix",
            "Set prefix for rewritten function predicates (default: F).",
            self._set_prefix,
            argument="<prefix>",
        )

    def _set_prefix(self, prefix: str) -> None:
        self._prefix = prefix

    def main(self, clingo_control: ClingoControl, files: Sequence[str]) -> None:
        prefix = self._prefix
        control = Control(
            self._library,
            self._clingo_options,
            prefix,
            clingo_control,
        )
        try:
            control.parse_files(files)
            if self._print_rewrite:
                print(control.get_rewritten_program())
                return
        except ParsingException as e:  # pragma: no cover
            for error in e.errors:
                sys.stderr.write(str(error) + "\n")
            sys.stderr.write("*** ERROR: (fasp): parsing failed")
            return
        control.main()


def fasp_main(
    library: ELibrary, options: list[str] | None = None, raise_errors: bool = False
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
        return clingo_main(library.library, options, app)
    except Exception:  # pragma: no cover
        if raise_errors:
            raise
        return 1


def main(options: Sequence[str] = ()) -> int:
    with ELibrary() as library:
        return fasp_main(library, list(options))
    return 1  # pragma: no cover
