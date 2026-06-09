# Funasp = Clingo + intensional functions

`funasp` extends `clingo 6` with intensional functions that can be defined by the user. The major new syntax expression of fasp are assignment rules.
The following is an example of an encoding of the graph coloring problem in `funasp`:
```prolog
color(X) := #some{r;g;b} :- country(X).
:- neighbor(C,D), color(C)=color(D).
```
More examples can be found in the folder `examples`.


# Funasp installations
```bash
conda create -n clingo6 python=3.13
conda activate clingo6
conda install -c potassco/label/dev-20 -c conda-forge clingo
git clone https://github.com/krr-uno/funasp.git
cd funasp
git pull
pip install -r requirements.txt
pip install -e .
funasp examples/family.lp
```