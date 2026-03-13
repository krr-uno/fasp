import ctypes
import sys
from typing import Callable, Sequence

from clingo import core, solve
from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.control import Control as ClingoControl

from funasp.__version__ import __version__
from funasp.control import Control
from funasp.solve import Model
from funasp.util.ast import ELibrary, ParsingException

LIBC_NAME: str | None = None
try:
    if sys.platform.startswith("win"): # pragma: no cover
        LIBC_NAME = "msvcrt"
except Exception as e: # pragma: no cover
    pass
LIBC = ctypes.CDLL(LIBC_NAME)  # None = current process's libc

# Declare fflush prototype
LIBC.fflush.argtypes = [ctypes.c_void_p]
LIBC.fflush.restype = ctypes.c_int


class FaspApp(App):
    def __init__(self, library: ELibrary, clingo_options: Sequence[str]) -> None:
        super().__init__("funasp", __version__)
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
            "prefix-fun",
            "Set prefix for rewritten function predicates (default: F).",
            self._set_prefix,
            argument="<prefix>",
        )

    def print_model(
        self, model: solve.Model, default_printer: Callable[[], None]
    ) -> None:
        LIBC.fflush(None)  # Flush C's stdout
        sys.stdout.write(str(Model(model, self._prefix)))
        sys.stdout.write("\n")
        sys.stdout.flush()

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
        except ParsingException as e:
            for error in e.errors:
                sys.stderr.write(str(error) + "\n")
            sys.stderr.write("*** ERROR: (fasp): parsing failed\n")
            return
        except RuntimeError as e:
            if "rewriting failed" == e.args[0]:
                sys.stdout.write("UNKNOWN\n")
                sys.stderr.write("*** ERROR: (fasp): rewriting failed\n")
                return
            raise e  # pragma: no cover
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
    # options.append("--outf=3")
    try:
        return clingo_main(library.library, options, app)
    except Exception:  # pragma: no cover
        if raise_errors:
            raise
        return 1


def main(options: Sequence[str] = ()) -> int:
    def logger(ty: core.MessageType, message: str) -> None:
        sys.stderr.write(message + "\n")

    with ELibrary(logger=logger) as library:
        return fasp_main(library, list(options))
    return 1  # pragma: no cover
