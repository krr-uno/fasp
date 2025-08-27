import sys
from typing import Any, Callable, Iterable, Optional, Sequence, Tuple

import clingo
from clingo import ast
from clingo.symbol import Symbol

from fasp.solve import Model

from .ast.parsing import parse_files
from .util.ast import StatementAST


class Control:

    def __init__(
        self,
        library: clingo.core.Library,
        options: Sequence[str] = (),
        prefix: str = "F",
        clingo_control: Optional[clingo.control.Control] = None,
    ):
        self.library = library
        self._options = options
        self.clingo_control = clingo_control or clingo.control.Control(library, options)
        self.prefix = prefix

    def program_from_parse_files(self, files: Sequence[str]) -> Sequence[StatementAST]:
        """
        Extend the logic program with a (non-ground) logic program in a file.

        Parameters
        ----------
        files
            The paths of the files to load.
        """
        _, statements = parse_files(
            self.library,
            files,
            self.prefix,
        )
        return statements

    def parse_files(self, files: Sequence[str]) -> None:
        """
        Extend the logic program with a (non-ground) logic program in a file.

        Parameters
        ----------
        file
            The path of the file to load.
        """
        statements = self.program_from_parse_files(files)
        program = ast.Program(self.library)
        for statement in statements:
            program.add(statement)
        self.clingo_control.join(program)

    def ground(
        self,
        parts: Sequence[Tuple[str, Sequence[Symbol]]] = (("base", ()),),
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
        *,
        async_: bool = False,
    ) -> Iterable[Model]:
        with self.clingo_control.solve(
            assumptions,
            None,
            on_unsat,
            on_stats,
            on_finish,
            yield_=True,
            async_=async_,
        ) as handle:
            for model in handle:
                yield Model(model, self.prefix)

    def main(self) -> None:
        """
        Main function to be called after parsing and grounding.
        """
        self.ground()
        for i, model in enumerate(self.solve()):
            sys.stdout.write(f"Answer {i + 1}:\n")
            sys.stdout.write(str(model) + "\n")
