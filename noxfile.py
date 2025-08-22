import nox

PYTHON_VERSIONS = False
# if "GITHUB_ACTIONS" in os.environ:
#     PYTHON_VERSIONS = [f"3.{i}" for i in range(13, 14)]

nox.options.sessions = "typecheck", "test", "format"
nox.options.default_venv_backend = None

PROJECT_NAME = "fasp"


@nox.session(python=PYTHON_VERSIONS)
# @nox.session
def test(session):
    """Run the test suite."""
    if session.python:
        session.install("clingo")
        session.install("coverage")
    session.run("coverage", "run", "-m", "unittest", "discover", "-v")
    session.run(
        "coverage",
        "report",
        "--sort=cover",
        "--fail-under=100",
        "-m",
    )


@nox.session(python=False)
def format(session):
    if session.python:
        max_version = max(v for v in PYTHON_VERSIONS)
        if max_version != session.python:
            return
        session.install("black", "isort", "autoflake")

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
    if session.python:
        session.install("pylint")
    session.run("pylint", PROJECT_NAME)
    pass


@nox.session(python=PYTHON_VERSIONS)
def typecheck(session):
    # session.install("mypy")
    session.run(
        "mypy", "--allow-redefinition-new", "--local-partial-types", "-p", PROJECT_NAME
    )
