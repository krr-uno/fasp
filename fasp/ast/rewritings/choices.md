# Choices

## Choices in Conventional ASP

A choice element has the form:
```prolog
{ a(X) : p(X) }.
```

For every X satisfying p(X), the atom a(X) may be included in an answer set, but it is optional.

### Forms of choice
- Simple choice: Two possible answer sets: `{}` or `{a}`.
```prolog
    { a }.
```
- Bounded choice: This rule says exactly one of `{a}` or `{b}` should be true.
```prolog
    {a; b} = 1.
```

- Choice with condition: Here, `p(1)` and `p(2)` are facts, so the choice is equivalent to `{ a(1), a(2) }`.

```prolog
    {a(X) : p(X) }.
    p(1..2).
```

- Bounded choice with condition: Here, `p(1)`, `p(2)` and `p(3)`, so the rule is equivalent to `{a(1); a(2); a(3)} = 1.`, so only one of `a(1)`, `a(2)` or `a(3)`  must be true.
```prolog
    {a(X) : p(X) } = 1.
    p(1..3).
```

- Bounded choice with guards: Here, `p(1)`, `p(2)` and `p(3)`, so the rule is equivalent to `{a(1); a(2); a(3)} < 2.`, so at most one, or none, of `a(1)`, `a(2)` or `a(3)`  can be true.
```prolog
    {a(X) : p(X) } < 2.
    p(1..3).
```

- Choice in body: Here, the rule says that `p` can be derived if either `a` or `b` or both are chosen.

```prolog
    p :- { a; b}.
```

---

## Choices in Functional ASP
Basic Syntax:
```prolog
{ f(X) := Y : Cond(X,Y) } = N.
```
From all possible pairs `(X,Y)`, select `N` assignments for the function `f(X) := Y`.

### Functional choice forms
- Either `a` is undefined or `1`.
```prolog
    { a := 1 }.
```
- Exactly one function either `a` or `b` takes the value `1`.
```prolog
    {a :=1 ;b := 1} = 1.
```
- For each value of X when `p(X)` holds, `f(X)` may be equal to `X`.
```prolog
    { f(X) := X : p(X) }.
```
-  Among all values of X when `p(X)` holds, `f(X)` takes on one value of `X`.
```prolog
    { f(X) := X : p(X) } = 1.
```

Using choice with functions, the solver will decide which function assignment holds.

## Translation from FASP to ASP
For a rule 
```prolog
    { f(X) := Y : Cond(X,Y) } = N.
```

We could add a new predicate `val_f(X,Y)` to represent the functional mapping of `f(X) := Y`.

Then, the rule becomes
 
```prolog
    { val_f(X,Y): Cond(X,Y) } = N.
```
which is valid in ASP.

However, since, `f(X)` can have at most one corresponding value of `Y`, we need to add a constraint to enforce functional consistency

```prolog
    :- val_f(X,Y1), val_f(X,Y2), Y1 != Y2.
```


Therefore, for the given rule, the equivalent program in ASP will be:
```prolog
{ val_f(X,Y): Cond(X,Y) } = N.
:- val_f(X,Y1), val_f(X,Y2), Y1 != Y2.
```

#### Some examples:

In ASP:
```prolog
    country(france).
    country(usa).
    person(felipe).

    {king(C,X) : person(X)}:- country(C).
    :- king(C,X1), king(C,X2), X1 != X2.

    :- king(C1,X), king(C2, X), C1 != C2.
```

Equivalent program in FASP:
```prolog
    country(france).
    country(usa).
    person(felipe).

    {king(C) := X : person(X)}:- country(C).

    :- king(C1,X), king(C2, X), C1 != C2.
```

---
In FASP:
```prolog
    p(1..3).
    { f(X) := X : p(X) } = 1.
```
Equivalent program in ASP:
```prolog
    p(1..3).
    { val_f(X,X) : p(X) } = 1.
    :- val_f(X,Y1), val_f(X,Y2), Y1 != Y2.
```