import sys


def check_versions() -> int:  # pragma: no cover
    """Validate the Python and clingo versions required by the CLI."""
    if sys.version_info < (3, 13):
        sys.stderr.write(
            f"*** ERROR: funasp requires Python 3.13 or higher, found version {sys.version_info} \n"
        )
        return 1
    try:
        from clingo_funasp import core  # pylint: disable=import-outside-toplevel

        clingo_version = core.version()
    except ImportError:
        sys.stderr.write(
            "*** ERROR: funasp requires clingo library version 6.0.0 or higher.\n"
        )
        return 1
    clingo_version_str = ".".join(map(str, clingo_version))
    if clingo_version < (6, 0, 0):
        sys.stderr.write(
            f"*** ERROR: funasp requires clingo library version 6.0.0 or higher, found version {clingo_version_str}.\n"
        )
        return 1
    return 0


def main() -> int:
    """Run the package entry point after validating the runtime environment."""
    if (error_code := check_versions()) != 0:  # pragma: no cover
        return error_code
    from funasp.app import main as app_main  # pylint: disable=import-outside-toplevel

    return app_main(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
