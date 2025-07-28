import os
import nox

PYTHON_VERSIONS = [f"3.{i}" for i in range(12, 13)]


@nox.session(python=PYTHON_VERSIONS)
def tests(session):
    """Run the test suite."""
    session.install("clingo")
    session.install("clingox")
    session.install("coverage")
    session.run("coverage", "run", "-m", "unittest", "discover", "-v")
    session.run(
        "coverage",
        "report",
        "--sort=cover",
        "--fail-under=99",
        "-m",
    )
    # with session.chdir("/home/jfandinno/git/python-clingox"):
    #     session.install("-e", ".")
    # session.run("python", "-m", "unittest", "discover", "-v")
