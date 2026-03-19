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
            "next(a)=c next(b)=a next(c)=f next(d)=e next(e)=b next(f)=d",
            "next(a)=c next(b)=e next(c)=b next(d)=a next(e)=f next(f)=d",
            "next(a)=d next(b)=a next(c)=b next(d)=e next(e)=f next(f)=c",
            "next(a)=d next(b)=c next(c)=a next(d)=f next(e)=b next(f)=e",
            "next(a)=b next(b)=c next(c)=f next(d)=a next(e)=d next(f)=e",
            "next(a)=b next(b)=e next(c)=a next(d)=f next(e)=d next(f)=c",
        ],
    ),
    Example(
        [EXAMPLES_PATH / "hamiltonian2.lp"],
        [
            "next(a)=c next(b)=a next(c)=f next(d)=e next(e)=b next(f)=d",
            "next(a)=c next(b)=e next(c)=b next(d)=a next(e)=f next(f)=d",
            "next(a)=d next(b)=a next(c)=b next(d)=e next(e)=f next(f)=c",
            "next(a)=d next(b)=c next(c)=a next(d)=f next(e)=b next(f)=e",
            "next(a)=b next(b)=c next(c)=f next(d)=a next(e)=d next(f)=e",
            "next(a)=b next(b)=e next(c)=a next(d)=f next(e)=d next(f)=c",
        ],
    ),
    Example(
        [EXAMPLES_PATH / "coloring.lp"],
        [
            "color(ch)=b color(de)=r color(fr)=g",
            "color(ch)=b color(de)=g color(fr)=r",
            "color(ch)=g color(de)=r color(fr)=b",
            "color(ch)=g color(de)=b color(fr)=r",
            "color(ch)=r color(de)=b color(fr)=g",
            "color(ch)=r color(de)=g color(fr)=b",
        ],
    ),
    Example(
        [EXAMPLES_PATH / "sudoku.lp"],
        [
            "cell(0,0)=4 cell(0,1)=2 cell(0,2)=7 cell(0,3)=5 cell(0,4)=6 cell(0,5)=8 cell(0,6)=1 cell(0,7)=3 cell(0,8)=9 cell(1,0)=9 cell(1,1)=1 cell(1,2)=5 cell(1,3)=3 cell(1,4)=4 cell(1,5)=2 cell(1,6)=8 cell(1,7)=7 cell(1,8)=6 cell(2,0)=6 cell(2,1)=8 cell(2,2)=3 cell(2,3)=1 cell(2,4)=9 cell(2,5)=7 cell(2,6)=5 cell(2,7)=4 cell(2,8)=2 cell(3,0)=8 cell(3,1)=7 cell(3,2)=1 cell(3,3)=9 cell(3,4)=2 cell(3,5)=6 cell(3,6)=4 cell(3,7)=5 cell(3,8)=3 cell(4,0)=3 cell(4,1)=4 cell(4,2)=9 cell(4,3)=8 cell(4,4)=5 cell(4,5)=1 cell(4,6)=2 cell(4,7)=6 cell(4,8)=7 cell(5,0)=2 cell(5,1)=5 cell(5,2)=6 cell(5,3)=4 cell(5,4)=7 cell(5,5)=3 cell(5,6)=9 cell(5,7)=1 cell(5,8)=8 cell(6,0)=7 cell(6,1)=6 cell(6,2)=4 cell(6,3)=2 cell(6,4)=1 cell(6,5)=9 cell(6,6)=3 cell(6,7)=8 cell(6,8)=5 cell(7,0)=5 cell(7,1)=9 cell(7,2)=8 cell(7,3)=7 cell(7,4)=3 cell(7,5)=4 cell(7,6)=6 cell(7,7)=2 cell(7,8)=1 cell(8,0)=1 cell(8,1)=3 cell(8,2)=2 cell(8,3)=6 cell(8,4)=8 cell(8,5)=5 cell(8,6)=7 cell(8,7)=9 cell(8,8)=4",
        ],
    ),
    Example(
        [EXAMPLES_PATH / "min_distance_edge_agg.lp"],
        [
            "dist(1,1)=0 dist(1,2)=1 dist(1,3)=1 dist(1,4)=2 dist(2,2)=0 dist(2,3)=1 dist(2,4)=2 dist(3,3)=0 dist(3,4)=1 dist(4,4)=0 dist(5,5)=0 dist(5,6)=1 dist(5,7)=2 dist(6,6)=0 dist(6,7)=1 dist(7,7)=0",
        ],
    ),
    Example(
        [EXAMPLES_PATH / "min_distance_edge.lp"],
        [
            "dist(1,1)=0 dist(1,2)=1 dist(1,3)=1 dist(1,4)=2 dist(2,2)=0 dist(2,3)=1 dist(2,4)=2 dist(3,3)=0 dist(3,4)=1 dist(4,4)=0 dist(5,5)=0 dist(5,6)=1 dist(5,7)=2 dist(6,6)=0 dist(6,7)=1 dist(7,7)=0",
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
    Example(
        [TEST_EXAMPLES_PATH / "unsat.lp"],
        [],
    ),
]

EXAMPLES = [
    Example(example.files, [textwrap.dedent(model).strip() for model in example.models])
    for example in EXAMPLES
]
