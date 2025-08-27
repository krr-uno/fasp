# fasp installations

clingo 6 API: https://potassco.org/clingo-preview/python-api/clingo.html

```bash
conda create -n clingo6 python=3.13
conda activate clingo6
conda install -c potassco/label/dev-20 -c conda-forge clingo
git clone https://github.com/krr-uno/fasp.git
cd fasp
git pull
pip install -e .
fasp examples/family.lp
```

# fasp

fasp extends clingo 6 with evaluable functions. The major new syntax expression of fasp are assignment rules.
These are rules of the forms:
```prolog
f(t1) = t2 :- Body.
{ f(t1) = t2 } :- Body.
f(t1) = #sum{ X : p(X) } :- Body.
```
where ```f``` is a function symbol and ```t1``` and ```t2``` are terms.
As an example, assignment fact 
```prolog
father(cain)=adam.
```
means that assigns the value ```adam``` to ```father(cain)```.


