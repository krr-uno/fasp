# Translate to clingo via flattening, rather than a dedicated solver

Status: accepted

funasp runs programs by translating them to plain ASP and solving with
*unmodified* clingo 6 (via its Python API), rather than building or forking a
solver as comparable systems did (ASPMT → z3, fasp → CSP, clingof → a clingo-3
fork). This reuses clingo's intelligent grounding, full aggregates, optimization,
and multi-shot solving for free; keeps the extension conservative (function-free
programs are unchanged); and — because the [flattening](../../CONTEXT.md) emits
explicit uniqueness constraints clingo exploits — actually solves
ASP-competition benchmarks ~10% faster than the hand-written clingo encodings.

## Considered options

- **Fork clingo** (like clingof): deeper integration, but a heavy maintenance
  burden against moving upstream.
- **SMT/CSP backend** (like ASPMT/fasp): strong for large numeric domains, but
  loses intelligent grounding, aggregates, and the conservative-extension
  guarantee.

## Consequences

Correctness now rests on the rewrite pipeline faithfully realizing the paper's
translation, and solver-level optimization for huge numeric domains is bounded
by clingo (left to future work).
