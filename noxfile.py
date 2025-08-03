import nox

PYTHON_VERSIONS = [f"3.{i}" for i in range(13, 14)]

nox.options.sessions = "lint", "typecheck", "test"
nox.options.default_venv_backend = None

PROJECT_NAME = "fasp"


# @nox.session(python=PYTHON_VERSIONS)
@nox.session
def test(session):
    """Run the test suite."""
    # session.install("clingo")
    # # session.install("clingox")
    # session.install("coverage")
    session.run("coverage", "run", "-m", "unittest", "discover", "-v")
    session.run(
        "coverage",
        "report",
        "--sort=cover",
        # "--fail-under=99",
        "-m",
    )


@nox.session
def format(session):
    # session.install("black", "isort", "autoflake")
    check = "check" in session.posargs

    autoflake_args = [
        "--in-place",
        "--imports=clingo",
        "--ignore-init-module-imports",
        "--remove-unused-variables",
        "-r",
        PROJECT_NAME,
    ]
    if check:
        autoflake_args.remove("--in-place")
    session.run("autoflake", *autoflake_args)

    isort_args = ["--profile", "black", PROJECT_NAME]
    if check:
        isort_args.insert(0, "--check")
        isort_args.insert(1, "--diff")
    session.run("isort", *isort_args)

    black_args = [PROJECT_NAME]
    if check:
        black_args.insert(0, "--check")
        black_args.insert(1, "--diff")
    session.run("black", *black_args)


@nox.session
def lint(session):
    # session.install("pylint")
    # session.run("pylint", PROJECT_NAME)
    pass


# @nox.session(python=PYTHON_VERSIONS)
@nox.session
def typecheck(session):
    # session.install("mypy")
    session.run("mypy", "-p", PROJECT_NAME)
