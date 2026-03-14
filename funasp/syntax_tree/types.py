from dataclasses import dataclass


@dataclass(frozen=True, slots=True, order=True)
class SymbolSignature:
    """
    Represents a function symbol with its name and arity.

    Attributes:
        name (str): The name of the function.
        arity (int): The number of arguments the function takes.
    """

    name: str
    arity: int

    def __str__(self) -> str:
        return f"{self.name}/{self.arity}"
