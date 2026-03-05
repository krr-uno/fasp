from clingo import ast
from functional_to_casp.util.ast import HeadLiteralAST

# Utility to negate a Relation
def negate_operator(op: ast.Relation) -> ast.Relation:
    # clingo.core.Relation
    rel = ast.Relation
    if op == rel.Equal:
        return rel.NotEqual
    elif op == rel.NotEqual:
        return rel.Equal
    elif op == rel.Less:
        return rel.GreaterEqual
    elif op == rel.LessEqual:
        return rel.Greater
    elif op == rel.Greater:
        return rel.LessEqual
    elif op == rel.GreaterEqual:
        return rel.Less
    else:
        raise ValueError(f"Unknown relation: {op}")

def is_constraint(rule_head: HeadLiteralAST) -> bool:
    if isinstance(rule_head, ast.HeadSimpleLiteral) and isinstance(rule_head.literal, ast.LiteralBoolean) and rule_head.literal.sign == ast.Sign.NoSign and  rule_head.literal.value == False:
        return True
    return False