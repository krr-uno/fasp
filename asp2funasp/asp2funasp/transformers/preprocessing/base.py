from asp2funasp.util.ast import StatementAST


class PreprocessingTransformer:
    def rewrite_rule(self, rule: StatementAST) -> StatementAST | None:
        return rule  # pragma: no cover
