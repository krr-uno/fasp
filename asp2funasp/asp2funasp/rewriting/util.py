from typing import Sequence

from asp2funasp.util.types import FRelation, SymbolSignature


class FreshFunctionNameGenerator:
    def __init__(self, reserved_names: Sequence[str]):
        self.used_names = set(reserved_names)

    def fresh(self, base_name: str) -> str:
        index = 1

        while True:
            candidate = f"{base_name}_{index}"

            if candidate not in self.used_names:
                self.used_names.add(candidate)
                return candidate

            index += 1


def build_function_name_index(
    frelations: Sequence[FRelation],
) -> dict[SymbolSignature, str]:
    """
    Decide which FUNASP function symbol name each FRelation should use.

    Normally:
        assign/2 -> assign

    Conflict case:
        assign/2 and assign/3 are both functional relations.

    Since assign/3 would be encoded as Fassign(N,C,V), this can conflict
    with assign/2 becoming a functional term assign(N,C). Therefore assign/3
    is emitted with a fresh base name:

        assign/3 -> assign_1
        emitted as Fassign_1(N,C,V)
    """
    signatures = {SymbolSignature(frel.name, frel.arity) for frel in frelations}

    generator = FreshFunctionNameGenerator(
        reserved_names=[frel.name for frel in frelations],
    )

    function_name_index: dict[SymbolSignature, str] = {}

    for frel in sorted(frelations, key=lambda item: (item.name, item.arity)):
        key = SymbolSignature(frel.name, frel.arity)
        previous_arity_key = SymbolSignature(frel.name, frel.arity - 1)

        if previous_arity_key in signatures:
            function_name_index[key] = generator.fresh(frel.name)
        else:
            function_name_index[key] = frel.name

    return function_name_index
