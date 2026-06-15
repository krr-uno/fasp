# funasp

The domain language of **funasp** — an extension of Answer Set Programming with
intensional functions and aggregates. Terms are anchored on the paper
*"Answer Set Programs with Intensional Functions and Aggregates"*
(`references/funasp.pdf`); the code's own names for the same concepts are listed
under `_Avoid_`.

## Language

### Functions

**Intensional function**:
A function whose values the programmer defines with rules (via assignments), the
way predicates are defined — not a built-in arithmetic or Herbrand
(term-constructor) function. May be partial and may be nested.
_Avoid_: evaluable function

**Rigid partial function**:
The semantics funasp gives every intensional function: a partial function that
is *rigid* — across the "here" and "there" worlds of the underlying
Here-and-There logic the here-world may leave a value undefined but may not
assign a different one. The same objects as an intensional function, named for
their logical reading.

**Extensional**:
A predicate or function *not* defined by program rules — built-ins and input
data. The complement of intensional.

**Undefined**:
The state of an intensional function at arguments where it has no value. `f(t)=v`
holds only when `f(t)` is defined and equals `v`, so `not f(t)=_` means "`f(t)`
is undefined" — making undefinedness observable and non-monotonic.

### Assignments and rules

**Assignment**:
A rule head `f(t) := t'` that defines the value of intensional function `f` at
arguments `t` to be `t'`. Contrast a body *equation* `f(t) = t'`, which tests a
value rather than defining one.

**Some-choice rule**:
A choice rule `f(t) := #some{...}` that gives `f(t)` one value from the
candidates if any exist and otherwise leaves `f(t)` undefined (staying
satisfiable).

**Braced-choice rule**:
A choice rule `l { Hd : ... } u` with optional bounds, in a syntax close to
regular ASP; `1{ f(t) := C : ... }` forces `f(t)` total and is unsatisfiable on
an empty candidate set — the key contrast with a some-choice rule.

**Aggregate assignment**:
An assignment whose right-hand side is an aggregate, e.g. `f(t) := #sum{X:p(X)}`,
defining the function value as the aggregate result.

### Programs and translation

**Answer set**:
A stable model of a funasp program, projected onto its predicate atoms `p(t)`
and function facts `f(t)=t'`. On function-free programs it coincides exactly with
clingo's, so funasp is a conservative extension.

**Plain program**:
A program in normal form where every intensional-function symbol appears only as
the left side of an assignment or equation, with no functions nested inside — the
form that can be flattened.

**Rough occurrence**:
A function term `f(t)` that is *not* the left side of an assignment or equation
(a nested or embedded use). A program is plain exactly when it has none; the code
calls removing them *unnesting*.

**Flattening**:
The translation of a plain program into a regular, function-free ASP program,
replacing each function `f/k` with a predicate `pf/(k+1)` plus a uniqueness
constraint; answer sets correspond one-to-one.

**Uniqueness constraint**:
The per-function constraint flattening adds to forbid two values for the same
arguments (`⊥ :- pf(X,Y), #count{Z:pf(X,Z)}>1`), enforcing functionality.
_Avoid_: functionality constraint
