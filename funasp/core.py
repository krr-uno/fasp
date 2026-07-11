"""
Core funasp utilities shared across the package.

Currently this hosts :class:`Library`, a wrapper around the clingo
``Library`` that captures and normalizes log messages (e.g. rewriting
"undefined predicate F..." into "undefined intensional function ...") and
carries the ``processing_statement`` text used in error reports.
"""

import re
import typing

from clingo_funasp.core import Library as ClingoLibrary
from clingo_funasp.core import LogLevel, MessageType

from funasp.util.types import SymbolSignature

_UNDEFINED_PREDICATE_REGEX = re.compile(r"undefined predicate (\S+)/(\d+)")


class Library:

    def __init__(
        self,
        *,
        shared: bool = True,
        slotted: bool = True,
        log_level: LogLevel = LogLevel.Info,
        logger: typing.Callable[[MessageType, str], None] | None = None,
        message_limit: int = 25,
    ) -> None:
        """Initialize the clingo library wrapper and its message handling state."""
        self.error_messages: list[tuple[MessageType, str]] = []
        self.logger = logger
        self.library = ClingoLibrary(
            shared,
            slotted,
            log_level,
            self.handle_log_message,
            message_limit,
        )
        self.prefix_function = "F"
        self.function_predicates: set[SymbolSignature] = set()
        self._processing_statement: str | None = None

    def processing_statement(self, statement: str) -> None:
        """Set the currently processing statement for more informative logging."""
        self._processing_statement = statement

    def clear_processing_statement(self) -> None:
        """Clear the currently processing statement."""
        self._processing_statement = None

    def handle_log_message(self, msg_type: MessageType, message: str) -> None:
        """Capture, normalize, and optionally forward messages emitted by clingo."""
        self.error_messages.append((msg_type, message))
        new_message = self.normalize_log_message(msg_type, message)
        if new_message and self.logger is not None:  # pragma: no cover
            self.logger(msg_type, new_message)

    def normalize_log_message(self, msg_type: MessageType, message: str) -> str | None:
        """Normalize selected clingo messages for FASP-specific reporting."""
        del msg_type
        if "unsafe variable" in message:
            lines = message.split("\n")
            if self._processing_statement is None:  # pragma: no cover
                lines[0] = lines[0][9:-3]
                lines.pop(1)
            else:
                lines[1] = f"  {self._processing_statement}"
            message = "\n".join(lines)
        elif (
            "operation undefined" in message and self._processing_statement is not None
        ):
            lines = message.split("\n")
            lines[0] = lines[0][:-1] + " in:"
            lines.insert(1, f"  {self._processing_statement}")
            lines.insert(2, "note: the following operations are undefined:")
            message = "\n".join(lines)
        elif (match := _UNDEFINED_PREDICATE_REGEX.search(message)) is not None:
            return self._normalize_undefined_predicate(message, match)
        return message

    def _normalize_undefined_predicate(
        self, message: str, match: re.Match[str]
    ) -> str | None:
        """Rewrite undefined-predicate messages about function encodings.

        Only predicates registered in ``function_predicates`` are rewritten;
        a user predicate whose name merely starts with the function prefix
        (reachable with ``--ignore-prefix-collisions``) is reported verbatim.
        """
        signature = SymbolSignature(match.group(1), int(match.group(2)))
        if signature not in self.function_predicates:
            return message
        if message.startswith("<functional>:0:0-0:"):
            return None
        name = signature.name[len(self.prefix_function) :]
        return message.replace(
            f"undefined predicate {signature}",
            f"undefined intensional function {name}/{signature.arity}",
        )

    def __enter__(self) -> typing.Self:
        """Enter the managed context and return this library wrapper."""
        return self

    def __exit__(
        self, exc_type: typing.Any, exc_value: typing.Any, traceback: typing.Any
    ) -> bool:
        """Exit the managed context by delegating to the underlying clingo library."""
        return self.library.__exit__(exc_type, exc_value, traceback)
