from pathlib import Path
import textwrap
from typing import NamedTuple

EXAMPLES_PATH = Path(__file__).parent.parent / "examples"


class Example(NamedTuple):
    files: list[Path]
    models: list[str]


EXAMPLES = [
    Example(
        [EXAMPLES_PATH / "ex01_asp.lp"],
        [
            "b(1) b(2) b(3)",
            "a b(1) b(2) b(3) c(1) c(2) c(3)",
        ],
    ),
    Example(
        [EXAMPLES_PATH / "ex02_fun.lp"],
        [
            """\
            b(1) b(2) b(3)
            c(1)=2 c(2)=3 c(3)=4
            """,
            """\
            a b(1) b(2) b(3)
            c(1)=2 c(2)=3 c(3)=4 d=2
            """,
        ],
    ),
]

EXAMPLES = [
    Example(example.files, [textwrap.dedent(model).strip() for model in example.models])
    for example in EXAMPLES
]
