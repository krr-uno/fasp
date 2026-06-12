from collections import namedtuple

from funasp.fun_ast.types import SymbolSignature as SymbolSignature

FPredicate = namedtuple(
    "FPredicate", ["name", "arity", "arguments", "values", "condition"]
)
CPredicate = namedtuple("CPredicate", ["name", "arity", "arguments"])
FRelation = namedtuple("FRelation", ["name", "arity", "arguments", "values"])
