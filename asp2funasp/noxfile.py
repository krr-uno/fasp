import os
from pathlib import Path

import nox

PYTHON_VERSIONS = False
# if "GITHUB_ACTIONS" in os.environ:
#     PYTHON_VERSIONS = [f"3.{i}" for i in range(13, 14)]

nox.options.sessions = "typecheck", "test", "format"
# nox.options.sessions = "test", "format"
nox.options.default_venv_backend = None

PROJECT_NAME = "asp2funasp"
PROJECT_ROOT = Path(__file__).resolve().parent
FUNASP_ROOT = PROJECT_ROOT.parent


def add_funasp_to_import_path(session):
    """Make the parent funasp package visible from the subproject sessions."""
    for key in ("PYTHONPATH", "MYPYPATH"):
        paths = [str(FUNASP_ROOT)]
        if existing := session.env.get(key, os.environ.get(key)):
            paths.append(existing)
        session.env[key] = os.pathsep.join(paths)


@nox.session(python=PYTHON_VERSIONS)
# @nox.session
def test(session):
    """Run the test suite."""
    add_funasp_to_import_path(session)
    if session.python:
        session.install("clingo")
        session.install("coverage")

    session.run(
        "coverage",
        "run",
        f"--source={PROJECT_NAME}",
        "-m",
        "unittest",
        "discover",
        # "-s", "tests",
        "-v",
    )
    session.run(
        "coverage",
        "report",
        "--sort=cover",
        "--fail-under=100",
        "-m",
        "--omit=tests/*",
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
    add_funasp_to_import_path(session)
    if session.python:
        session.install("pylint")
    session.run("pylint", PROJECT_NAME)


@nox.session(python=PYTHON_VERSIONS)
def typecheck(session):
    # session.install("mypy")
    add_funasp_to_import_path(session)
    session.run(
        "mypy",
        "--allow-redefinition-new",
        "--local-partial-types",
        "--explicit-package-bases",
        "--follow-imports=silent",
        "--strict",
        PROJECT_NAME,
    )


@nox.session(python=PYTHON_VERSIONS)
def typecheckT(session):
    # session.install("mypy")
    add_funasp_to_import_path(session)
    session.run(
        "mypy",
        "--allow-redefinition-new",
        "--local-partial-types",
        "--explicit-package-bases",
        "--follow-imports=silent",
        "--strict",
        "tests",
    )
