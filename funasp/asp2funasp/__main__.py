"""Run the standalone ASP-to-FUNASP converter as a module."""

import sys

from funasp.asp2funasp.cli import main

if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
