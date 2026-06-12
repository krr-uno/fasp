from clingo import ast


class PreprocessingTransformer:
    def rewrite_rule(self, rule: ast.Statement) -> ast.Statement | None:
        return rule  # pragma: no cover
