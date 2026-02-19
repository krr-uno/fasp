from pathlib import Path
import textwrap
from typing import NamedTuple

EXAMPLES_PATH = Path(__file__).parent.parent / "examples"
TEST_EXAMPLES_PATH = Path(__file__).parent / "examples"


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
    Example(
        [EXAMPLES_PATH / "aggregate_assignment.lp"],
        [
            """\
            intersection_count=2
            """,
        ],
    ),
    Example(
        [EXAMPLES_PATH / "family.lp"],
        [
            """\
            female(eve) male(adam) orphan(adam) orphan(eve) person(abel) person(adam) person(cain) person(eve)
            father(abel)=adam father(cain)=adam mother(abel)=eve mother(cain)=eve n_orphan=2
            """,
        ],
    ),
    Example(
        [EXAMPLES_PATH / "fib.lp"],
        [
            """\
            fibo(0)=1 fibo(1)=1 fibo(10)=89 fibo(2)=2 fibo(3)=3 fibo(4)=5 fibo(5)=8 fibo(6)=13 fibo(7)=21 fibo(8)=34 fibo(9)=55
            """,
        ],
    ),
       Example(
        [EXAMPLES_PATH / "hamiltonian.lp"],
        [
            """\
            female(eve) male(adam) orphan(adam) orphan(eve) person(abel) person(adam) person(cain) person(eve)
            father(abel)=adam father(cain)=adam mother(abel)=eve mother(cain)=eve n_orphan=2
            """,
        ],
    ),
    Example(
        [TEST_EXAMPLES_PATH / "showf.lp"],
        [
            """\
            a=1
            """,
        ],
    ),
]

EXAMPLES = [
    Example(example.files, [textwrap.dedent(model).strip() for model in example.models])
    for example in EXAMPLES
]
