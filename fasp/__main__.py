import sys

from fasp.__version__ import __version__


def check_versions(args) -> int:
    if sys.version_info < (3, 13):
        sys.stderr.write(
            f"*** ERROR: fasp requires Python 3.13 or higher, found version {sys.version_info} \n"
        )
        return 1
    try:
        from clingo import core

        clingo_version = core.version()
    except ImportError:
        sys.stderr.write(
            "*** ERROR: fasp requires clingo library version 6.0.0 or higher.\n"
        )
        return 1
    clingo_version_str = ".".join(map(str, clingo_version))
    if clingo_version < (6, 0, 0):
        sys.stderr.write(
            f"*** ERROR: fasp requires clingo library version 6.0.0 or higher, found version {clingo_version_str}.\n"
        )
        return 1
    sys.stdout.write(f"clingo version {clingo_version_str}\n")
    return 0


def main() -> int:
    args = frozenset(sys.argv[1:])
    version_mode = "-v" in args or "--version" in args
    if not version_mode:
        sys.stdout.write(f"fasp version {__version__}\n")
    if error_code := check_versions(args) != 0:
        return error_code
    if not version_mode:
        sys.stdout.write("\n")
    from fasp.app import main as app_main

    return app_main(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
