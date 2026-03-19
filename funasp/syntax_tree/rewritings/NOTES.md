The rewritten from functional ASP into standard ASP is obtained by applying a sequence of transformers
- rewrite away assignments with aggregates, this requires collecting all variables
- collect all evaluable functions
- unnest evaluable functions, top evaluable functions in some comparisons are considered nested
- rewrite evaluable functions as predicates