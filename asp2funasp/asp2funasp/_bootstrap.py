import sys
from pathlib import Path


def add_workspace_root_to_path() -> None:
    workspace_root = str(Path(__file__).resolve().parents[2])
    if workspace_root not in sys.path:
        sys.path.append(workspace_root)
