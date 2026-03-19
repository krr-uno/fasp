import ctypes
import sys
from typing import Callable, Optional, Sequence

from clingo import core, solve
from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.control import Control as ClingoControl
from clingo.control import ControlMode as ClingoControlMode
from colorama import Fore, Style
from colorama import deinit as colorama_deinit
from colorama import init as colorama_init

from funasp.__version__ import __version__
from funasp.control import Control
from funasp.util.ast import ELibrary, ParsingException

LIBC_NAME: str | None = None
try:
    if sys.platform.startswith("win"):  # pragma: no cover
        LIBC_NAME = "msvcrt"
except Exception as e:  # pragma: no cover
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
        self._control: Optional[Control] = None
        self._errors: list[Exception] = []

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
        assert self._control is not None
        self._control.print_model(model, default_printer)

    def _set_prefix(self, prefix: str) -> None:
        self._prefix = prefix

    def main(self, clingo_control: ClingoControl, files: Sequence[str]) -> None:
        prefix = self._prefix
        self._control = Control(
            self._library,
            self._clingo_options,
            prefix,
            clingo_control,
        )
        try:
            self._control.parse_files(files)
            if clingo_control.mode == ClingoControlMode.Rewrite:
                print(self._control.get_rewritten_program())
                return
        except ParsingException as e:
            for error in e.errors:
                sys.stderr.write(str(error) + "\n")
            self._errors.append(e)
            return
        except RuntimeError as e:
            if "rewriting failed" == e.args[0]:
                self._errors.append(e)
                return
            raise e  # pragma: no cover
        self._control.main()

    def report_error_summary(self) -> None:
        if any(isinstance(error, ParsingException) for error in self._errors):
            print(
                Style.BRIGHT
                + Fore.RED
                + "*** ERROR: (fasp):"
                + Style.RESET_ALL
                + " parsing failed\n",
                file=sys.stderr,
            )
        if any(
            isinstance(error, RuntimeError) and error.args[0] == "rewriting failed"
            for error in self._errors
        ):
            print(
                Style.BRIGHT
                + Fore.RED
                + "*** ERROR: (fasp):"
                + Style.RESET_ALL
                + " rewriting failed\n",
                file=sys.stderr,
            )


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
    colorama_init(autoreset=True)
    if options is None:  # pragma: no cover
        options = []
    app = FaspApp(library, options)
    # options.append("--outf=3")
    try:
        result = clingo_main(library.library, options, app)
        app.report_error_summary()
        return result
    except Exception:  # pragma: no cover
        if raise_errors:
            raise
        return 1
    colorama_deinit()  # pragma: no cover


def main(options: Sequence[str] = ()) -> int:
    def logger(ty: core.MessageType, message: str) -> None:
        sys.stderr.write(message + "\n")

    with ELibrary(logger=logger) as library:
        return fasp_main(library, list(options))
    return 1  # pragma: no cover
