# Funasp = Clingo + intensional functions

System `funasp` extends [clingo 6](https://github.com/potassco/clingo/tree/wip-20) with intensional functions that can be defined by the user. The major new syntax expression of `funasp` are assignment rules.
The following is an example of an encoding of the graph coloring problem in `funasp`:
```prolog
color(X) := #some{r;g;b} :- country(X).
:- neighbor(C,D), color(C)=color(D).
```
More examples can be found in the folder `examples`.


# Funasp installations

Funasp requires `python 3.13` or later and can be installed using `pip`.
```bash
pip install funasp
funasp examples/family.lp
```