import sys
from typing import Callable, Optional, Sequence

from clingo_funasp import core, solve
from clingo_funasp.app import App, AppOptions, Flag, clingo_main
from clingo_funasp.control import Control as ClingoControl
from clingo_funasp.control import ControlMode as ClingoControlMode
from colorama import Fore, Style
from colorama import deinit as colorama_deinit
from colorama import init as colorama_init

from funasp.__version__ import __version__
from funasp.control import Control
from funasp.core import Library
from funasp.util.ast import ParsingException, RewritingException

VALID_PREFIXES = frozenset("ABCDEFGHIJKLMNOPQRSTUVWXYZ")


def valid_function_prefix(prefix: str) -> bool:
    """Return whether a function predicate prefix is safe for rewriting."""
    return len(prefix) == 1 and prefix in VALID_PREFIXES


class FaspApp(App):
    def __init__(self, library: Library, clingo_options: Sequence[str]) -> None:
        """Initialize the FaspApp instance."""
        super().__init__("funasp", __version__)
        self._order = Flag()
        self._library = library
        self._clingo_options = clingo_options
        self._prefix = "F"
        self._print_rewrite = False
        self._control: Optional[Control] = None
        self._errors: list[Exception] = []
        self._option_errors: list[ValueError] = []

    def register_options(self, options: AppOptions) -> None:
        """Register the command-line options supported by this application."""
        options.add_flag(
            "fasp", "order", "Print atoms in models in order.", self._order
        )

        options.add(
            "fasp",
            "prefix-fun",
            "Set prefix for rewritten function predicates "
            "(single uppercase letter, default: F).",
            self._set_prefix,
            argument="<prefix>",
        )

    def print_model(
        self, model: solve.Model, default_printer: Callable[[], None]
    ) -> None:
        """Delegate model printing to the wrapped control object."""
        assert self._control is not None
        self._control.print_model(model, default_printer)

    def _set_prefix(self, prefix: str) -> None:
        """Store the prefix used for rewritten function predicates."""
        if not valid_function_prefix(prefix):
            self._option_errors.append(
                ValueError(
                    "--prefix-fun must be a single uppercase letter "
                    f"(got {prefix!r})"
                )
            )
            return
        self._prefix = prefix

    def main(self, clingo_control: ClingoControl, files: Sequence[str]) -> None:
        """Parse the input files and either print the rewrite or run solving."""
        if self._option_errors:
            for error in self._option_errors:
                sys.stderr.write(f"error: {error}\n")
            self._errors.extend(self._option_errors)
            return

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
        except RewritingException as e:
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

    @property
    def has_errors(self) -> bool:
        """Return whether parsing or rewriting recorded any errors."""
        return bool(self._errors)

    def report_error_summary(self) -> None:
        """Print a short summary for parsing or rewriting failures."""
        if any(isinstance(error, ParsingException) for error in self._errors):
            print(
                Style.BRIGHT
                + Fore.RED
                + "*** ERROR: (fasp):"
                + Style.RESET_ALL
                + " parsing failed",
                file=sys.stderr,
            )
        if any(
            isinstance(error, RewritingException)
            or (isinstance(error, RuntimeError) and error.args[0] == "rewriting failed")
            for error in self._errors
        ):
            print(
                Style.BRIGHT
                + Fore.RED
                + "*** ERROR: (fasp):"
                + Style.RESET_ALL
                + " rewriting failed",
                file=sys.stderr,
            )


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

    Returns
    -------
    int
        The clingo exit code, or 65 if parsing or rewriting failed.
    """
    colorama_init(autoreset=True)
    if options is None:  # pragma: no cover
        options = []
    app = FaspApp(library, options)
    # options.append("--outf=3")
    try:
        result = clingo_main(library.library, options, app)
        app.report_error_summary()
        if app.has_errors:
            return 65
        return result
    except BaseException:  # pragma: no cover
        if raise_errors:
            raise
        return 1
    finally:
        colorama_deinit()


def main(options: Sequence[str] = ()) -> int:
    """Create the shared library wrapper and run the CLI application."""

    def logger(ty: core.MessageType, message: str) -> None:
        """Forward clingo log messages to standard error."""
        sys.stderr.write(message + "\n")

    with Library(logger=logger) as library:
        return fasp_main(library, list(options))
    return 1  # pragma: no cover
