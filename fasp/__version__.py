from importlib.metadata import version, PackageNotFoundError

try:
    __version__ = version("fasp")
except PackageNotFoundError:
    __version__ = "0.0.0"