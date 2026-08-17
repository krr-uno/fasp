import sys
from collections.abc import Callable
from typing import Any, Iterable, Optional, Sequence

import clingo_funasp
from clingo_funasp import ast
from clingo_funasp import solve as clingo_solve
from clingo_funasp import symbol

from funasp.asp2funasp import ConversionResult, convert_statements
from funasp.ast import (
    RewriteContext,
    Statement,
    parse_files,
    parse_string,
    rewrite_statements,
)
from funasp.core import Library
from funasp.solve import Model


class Control:

    def __init__(
        self,
        library: Library,
        options: Sequence[str] = (),
        prefix: str = "F",
        clingo_control: Optional[clingo_funasp.control.Control] = None,
        ignore_prefix_collisions: bool = False,
        asp2funasp: bool = False,
    ):
        """Initialize the Control instance."""
        self.library = library
        self.clingo_control = clingo_control or clingo_funasp.control.Control(
            library.library, options
        )
        self.prefix = prefix
        self.library.prefix_function = prefix
        self.ignore_prefix_collisions = ignore_prefix_collisions
        self.asp2funasp = asp2funasp
        self._conversion_result: ConversionResult | None = None
        self._rewritten_program: Optional[str] = None
        self._result: Optional[clingo_funasp.solve.SolveResult] = None

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
        self._load(parse_files(self.library, files))

    def parse_string(self, code: str) -> None:
        """
        Parse the given FASP string, rewrite it to clingo AST statements, and
        add the result to the underlying clingo control. Also stores the
        rewritten program string for later retrieval via ``get_rewritten_program``.

        Parameters
        ----------
        code
            The FASP program text to parse and load.
        """
        self._load(parse_string(self.library, code))

    def _load(self, statements: list[Statement]) -> None:
        """Rewrite parsed statements and load them into the clingo control."""
        statements = self._convert_asp2funasp(statements)
        rewrite_context = RewriteContext(
            self.library,
            self.prefix,
            ignore_prefix_collisions=self.ignore_prefix_collisions,
        )
        rewritten = rewrite_statements(rewrite_context, statements)
        program = ast.Program(self.library.library)
        for wrapper in rewritten:
            for statement in wrapper.rewritten:
                program.add(statement)
        self.clingo_control.join(program)
        self._rewritten_program = "\n".join(
            str(statement) for wrapper in rewritten for statement in wrapper.rewritten
        )

    def _convert_asp2funasp(self, statements: list[Statement]) -> list[Statement]:
        """Convert parsed ASP statements when asp2funasp mode is enabled."""
        if not self.asp2funasp:
            return statements

        self._conversion_result = convert_statements(
            self.library.library,
            [statement.original for statement in statements],
        )
        return [
            Statement(self.library.library, statement)
            for statement in self._conversion_result.converted_statements
        ]

    @property
    def conversion_result(self) -> ConversionResult | None:
        """Return metadata from the most recent asp2funasp conversion, if any."""
        return self._conversion_result

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
        assumptions: Sequence[tuple[clingo_funasp.symbol.Symbol, bool] | int] = (),
        on_unsat: Callable[[Sequence[int]], None] | None = None,
        on_stats: (
            Callable[[clingo_funasp.stats.Stats, clingo_funasp.stats.Stats], None]
            | None
        ) = None,
        on_finish: Callable[[clingo_funasp.solve.SolveResult], None] | None = None,
    ) -> Iterable[Model]:
        """Solve the current grounded program and yield wrapped FASP models."""
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
        """Print a model using the FASP-aware model formatter."""
        sys.stdout.write(str(Model(model, self.prefix)))
        sys.stdout.write("\n")

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

        Raises
        ------
        ValueError
            If no program has been parsed yet.
        """
        if self._rewritten_program is None:
            raise ValueError("No program has been parsed yet.")
        return self._rewritten_program
