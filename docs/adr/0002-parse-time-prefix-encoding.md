# Encode function syntax with a parse-time name prefix

Status: accepted

funasp desugars its function syntax (assignments, choice and aggregate
assignments, `#some`) into ordinary clingo AST *at parse time* inside the
`clingo_funasp` parser fork, encoding function names with an uppercase prefix
(`F`, and `FS` for `#some`); all semantics is then handled separately in
`funasp/ast/_rewritings/`. The encoding is unambiguous because user function names
cannot start with an uppercase letter, and it lets the pipeline reuse clingo's
AST wholesale instead of defining custom node types.

## Considered options

- **Custom AST node types**: most explicit, but requires extending every visitor
  and printer and diverging further from clingo's AST.
- **Theory atoms**: avoid a parser fork, but the syntax is restricted and unnatural
  for assignments.
- **Textual preprocessing**: brittle and loses source locations needed for error
  messages.

## Consequences

The project carries a clingo parser fork to track upstream, and the `F`/`FS`
prefixes are a convention shared by string-match between the parser and the
rewriter — grounding the parser output without rewriting deliberately yields
wrong answers.
