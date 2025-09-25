import typing

from clingo.core import Library, LogLevel, MessageType


class FaspLibrary:

    def __init__(
        self,
        shared: bool = True,
        slotted: bool = True,
        log_level: LogLevel = LogLevel.Info,
        logger: typing.Callable[[MessageType, str], None] | None = None,
        message_limit: int = 25,
    ) -> None:
        self.error_messages: list[tuple[MessageType, str]] = []
        self.shared = shared
        self.slotted = slotted
        self.log_level = log_level
        self.logger = logger
        self.message_limit = message_limit
        self.library = Library(
            shared,
            slotted,
            log_level,
            self.logger_function,
            message_limit,
        )

    def logger_function(self, msg_type: MessageType, message: str) -> None:
        if self.logger is not None:
            self.logger(msg_type, message)
        self.error_messages.append((msg_type, message))

    def __enter__(self) -> typing.Self:
        return self

    def __exit__(self, exc_type: typing.Any, exc_value: typing.Any, traceback: typing.Any) -> bool:
        return self.library.__exit__(exc_type, exc_value, traceback)
