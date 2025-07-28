from typing import Any, Callable, Iterable, Optional, Sequence, Tuple, Union

import clingo
from clingo import Logger, SolveResult, StatisticsMap, Symbol, ast
from clingo.ast import ProgramBuilder

from fasp.solving import Model

from .ast.parsing import parse_files


class Control:

    def __init__(
        self,
        arguments: Sequence[str] = [],
        logger: Optional[Logger] = None,
        message_limit: int = 20,
        prefix: str = "F",
    ):
        self.logger = logger
        self.message_limit = message_limit
        self.clingo_control = clingo.Control(arguments, logger, message_limit)
        self.prefix = prefix

    def load(self, file: str) -> None:
        """
        Extend the logic program with a (non-ground) logic program in a file.

        Parameters
        ----------
        file
            The path of the file to load.
        """
        _, statements = parse_files(
            [file], self.logger, self.message_limit, self.prefix
        )
        with ProgramBuilder(self.clingo_control) as builder:
            for statement in statements:
                builder.add(statement)

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
        assumptions: Sequence[Union[Tuple[Symbol, bool], int]] = (),
        on_unsat: Optional[Callable[[Sequence[int]], None]] = None,
        on_statistics: Optional[Callable[[StatisticsMap, StatisticsMap], None]] = None,
        on_finish: Optional[Callable[[SolveResult], None]] = None,
        on_core: Optional[Callable[[Sequence[int]], None]] = None,
        *,
        async_: bool = False,
    ) -> Iterable[Model]:
        with self.clingo_control.solve(
            assumptions,
            on_unsat,
            on_statistics,
            on_finish,
            on_core,
            yield_=True,
            async_=async_,
        ) as handle:
            for model in handle:
                yield Model(model, self.prefix)
