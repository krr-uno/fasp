# Funasp = Clingo + intensional functions

System `funasp` extends [clingo 6](https://github.com/potassco/clingo/tree/wip-20) with intensional functions that can be defined by the user. The major new syntax expression of `funasp` are assignment rules.
The following is an example of an encoding of the graph coloring problem in `funasp`:
```prolog
color(X) := #some{r;g;b} :- country(X).
:- neighbor(C,D), color(C)=color(D).
```
More examples can be found in the folder `examples`.


## Funasp installation

Funasp requires `python 3.13` or later and can be installed using `pip`.
```bash
pip install funasp
funasp examples/family.lp
```

## Language

`funasp` is a strict superset of the clingo language: every clingo program is a
valid `funasp` program. On top of it, `funasp` adds *intensional functions* —
functions whose values are defined by the rules of the program. Function names
must start with a lowercase letter (as ordinary functions do).

### Assignment rules

The headline construct is the **assignment rule**, which defines the value of a
function. The general form is `f(t1,...,tn) := v :- Body.`, read as "the value of
`f(t1,...,tn)` is `v` whenever `Body` holds".

```prolog
father(cain) := adam.
x := 2 :- a.
fibo(X) := fibo(X-1) + fibo(X-2) :- number(X), X>1.
```

The function value is uniques: for each tuple of arguments it takes at most one
value. Defining two different values for the same arguments is unsatisfiable.

### Choice assignments

### `#some` assignments

Nondeterminitic assignments can be specified by
`#some{ ... }` choice rules.
```prolog
color(X) := #some{r;g;b} :- country(X).        % each country gets one color
cell(X,Y) := #some{1..9} :- X=0..8, Y=0..8.    % each cell gets one digit
next(X) := #some{ Y : edge(X,Y) } :- vertex(X).
```
A `#some{ ... }` choice rule state that **exactly one** value from its set is nondeterministically assigned to the function, if its set is non-empty.

Alternatviely, we can use the usual syntax clingo choice rules by wrapping the head in braces turns the assignment into a *choice*: the function
may take the value or be left undefined.

```prolog
{ usedcoins(V) := 1..N } :- coins(V)=N.        % optionally assign a value in 1..N
```

Choice assignments combine with cardinality bounds, exactly as clingo choice
rules do, to force a function to be defined:

```prolog
{ king(C) := X : citizen(X,C) } = 1 :- monarchy(C).
1 { move_to(B,T+1) := L : clear(B,T), location(L), L!=B } 1 :- step(T).
```

### Aggregate assignments

The value of a function can be defined by an aggregate (`#count`, `#sum`,
`#min`, `#max`):

```prolog
intersection_count := #count{ X : p(X), q(X), r(X) }.
n_orphan := #count{ X : orphan(X) }.
controller(C3) := C1 :- company(C1), company(C3),
                        #sum{ controlsStk(C1,C2,C3), C2 } > 50.
```



### Functions in rule bodies

Intensional functions can be used as **terms** anywhere in a body, and may be
nested inside other functions or arithmetic:

```prolog
:- neighbor(C,D), color(C)=color(D).       % compare two function values
visited(next(start)).                       % nested function term
:- on(B,last) != goal(B).                  % inequality between values
```

The comparison `f(t)=_` tests whether `f(t)` is defined, and combines with
negation:

```prolog
block(B) :- on(B,0)=_.                                  % on(B,0) is defined
orphan(X) :- person(X), not father(X)=_, not mother(X)=_.
```

### Showing functions

The `#showf` directive prints function atoms back in function syntax (`f(t)=v`)
instead of the internal predicate representation:

```prolog
#showf color/1.
#showf intersection_count/0.
```

More examples covering each of these constructs can be found in the `examples`
folder.