import ctypes
import sys
from collections.abc import Callable
from typing import Any, Iterable, Optional, Sequence

import clingo
from clingo import ast
from clingo import solve as clingo_solve
from clingo import symbol

from funasp.solve import Model
from funasp.syntax_tree import rewrite_statements
from funasp.syntax_tree._context import RewriteContext
from funasp.syntax_tree.parsing import parser
from funasp.util.ast import ELibrary

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


class Control:

    def __init__(
        self,
        library: ELibrary,
        options: Sequence[str] = (),
        prefix: str = "F",
        clingo_control: Optional[clingo.control.Control] = None,
    ):
        self.library = library
        self.clingo_control = clingo_control or clingo.control.Control(
            library.library, options
        )
        self.prefix = prefix
        self._rewritten_program: Optional[str]
        self._result: Optional[clingo.solve.SolveResult] = None

    def parse_files(self, files: Sequence[str]) -> None:
        """
        Parse the given FASP files, rewrite them to clingo AST statements, and
        add the result to the underlying clingo control. Also stores the
        rewritten program string for later retrieval via ``get_rewritten_program``.

        Parameters
        ----------
        files
            The paths of the files to parse and load.
        """
        rewrite_ctx = RewriteContext(self.library, self.prefix)
        statements = parser.parse_files(self.library, files)
        statements = rewrite_statements(rewrite_ctx, statements)
        program = ast.Program(self.library.library)
        for statement in statements:
            program.add(statement)
        self.clingo_control.join(program)
        self._rewritten_program = "\n".join(str(s) for s in statements)

    def ground(
        self,
        parts: Sequence[tuple[str, Sequence[symbol.Symbol]]] = (("base", ()),),
        context: Any = None,
    ) -> None:
        """
        Ground the given list of program parts specified by tuples of names and
        arguments.

        Parameters
        ----------
        parts
            List of tuples of program names and program arguments to ground.
        context
            A context object whose methods are called during grounding using
            the `@`-syntax (if omitted, those from the main module are used).

        Notes
        -----
        Note that parts of a logic program without an explicit `#program`
        specification are by default put into a program called `base` without
        arguments.
        """
        return self.clingo_control.ground(parts, context)

    def solve(
        self,
        assumptions: Sequence[tuple[clingo.symbol.Symbol, bool] | int] = (),
        on_unsat: Callable[[Sequence[int]], None] | None = None,
        on_stats: (
            Callable[[clingo.stats.Stats, clingo.stats.Stats], None] | None
        ) = None,
        on_finish: Callable[[clingo.solve.SolveResult], None] | None = None,
    ) -> Iterable[Model]:
        with self.clingo_control.start_solve(
            assumptions=assumptions,
            on_unsat=on_unsat,
            on_stats=on_stats,
            on_finish=on_finish,
            yield_=True,
        ) as handle:
            for model in handle:
                yield Model(model, self.prefix)
            self._result = handle.get()

    def print_model(self, model: clingo_solve.Model, _: Callable[[], None]) -> None:
        LIBC.fflush(None)  # Flush C's stdout
        sys.stdout.write(str(Model(model, self.prefix)))
        sys.stdout.write("\n")
        sys.stdout.flush()

    def main(self) -> None:
        """
        Main function to be called after parsing and grounding.
        """
        self.clingo_control.main()

    def get_rewritten_program(self) -> str:
        """
        Get the rewritten ASP program as a string.

        Returns
        -------
        str
            The rewritten ASP program.
        """
        if self._rewritten_program is None:
            return "No program has been parsed yet."  # pragma: no cover
            raise ValueError("No program has been parsed yet.")
        return self._rewritten_program
