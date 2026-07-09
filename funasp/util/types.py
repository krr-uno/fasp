# from dataclasses import dataclass
from typing import Final, NamedTuple

#: Predicate-name prefixes reserved for pipeline-generated auxiliary
#: predicates. ``RewriteContext.fresh_predicate_name`` names auxiliaries with
#: the first entry that cannot be mistaken for the function prefix, and model
#: output (``funasp.solve``) hides atoms starting with any of them — the two
#: uses must agree, so both read this constant.
AUXILIARY_PREDICATE_PREFIXES: Final = ("RD", "AD")


class SymbolSignature(NamedTuple):
    """
    Represents a function symbol with its name and arity.

    Attributes:
        name (str): The name of the function.
        arity (int): The number of arguments the function takes.
    """

    name: str
    arity: int

    def __str__(self) -> str:
        """Return the string form of the signature as name/arity."""
        return f"{self.name}/{self.arity}"
