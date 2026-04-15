import textwrap
import unittest

from typing import Tuple, Dict

from clingo import ast
from clingo.core import Library

from asp2funasp.util import ast as ast_util
from asp2funasp.util.types import FRelation, SymbolSignature

from tests.util import find_in_ast, parse_and_find, collect_statements


class UtilTest(unittest.TestCase):
    def setUp(self) -> None:
        self.lib = Library()

    def test_function_pools(self) -> None:
        node = parse_and_find(self.lib, "a((1,b)).", ast.TermTuple)
        _,_ = ast_util.function_arguments(node)