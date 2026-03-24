Hi, I get that the text printed by clingo 6 and the one printed from Python get desynchronized. For instance, running
```
echo "{a;b} = 1." | python myclingo.py --order 0
```
where myclingo.py is the example in the docs:
```python
from typing import Callable, Sequence
import sys
import ctypes
from clingo.app import App, AppOptions, Flag, clingo_main
from clingo.core import Library
from clingo.control import Control
from clingo.solve import Model
from clingo.symbol import Symbol

Parts = Sequence[Sequence[tuple[str, Sequence[Symbol]]]]

class MyApp(App):
    def __init__(self) -> None:
        super().__init__("my-app", "1.0.0")
        self._order = Flag()

    def print_model(self, model: Model, default_printer: Callable[[], None]) -> None:
        if self._order.value:
            print(" ".join(str(sym) for sym in sorted(model.symbols(shown=True))))
        else:
            default_printer()

    def register_options(self, options: AppOptions) -> None:
        options.add_flag(
            "MyApp", "order", "Print atoms in models in order.", self._order
        )

    def main(self, control: Control, files: Sequence[str]) -> None:
        control.parse_files(files)
        sys.stdout.flush()
        control.main()

with Library() as lib:
    sys.exit(clingo_main(lib, sys.argv[1:], MyApp()))
```
I get the expected result:
```
my-app version 1.0.0
Reading from stdin
Solving...
Answer: 1 (Time: 0.001s)
b
Answer: 2 (Time: 0.001s)
a
SATISFIABLE
Models : 2
Calls : 1
Time : 0.001s (Solving: 0.000s 1st Model: 0.000s Unsat: 0.000s)
CPU Time : 0.001s
```
However, if I redirect the output to a file ```echo "{a;b} = 1." | python myclingo.py --order 0 > out.txt```, then I get
```
my-app version 1.0.0
Reading from stdin
Solving...
Answer: 1 (Time: 0.000s)
Answer: 2 (Time: 0.001s)
SATISFIABLE
Models : 2
Calls : 1
Time : 0.001s (Solving: 0.000s 1st Model: 0.000s Unsat: 0.000s)
CPU Time : 0.001s
b
a
```
This use to happen in a previous version of clingo and was later fixed. Adding ```sys.stdout.flush()``` used to be a workaround, but that does not work now. It makes python appear before clingo text:
```
my-app version 1.0.0
Reading from stdin
b
Solving...
Answer: 1 (Time: 0.001s)
a
Answer: 2 (Time: 0.001s)
SATISFIABLE
Models : 2
Calls : 1
Time : 0.001s (Solving: 0.000s 1st Model: 0.000s Unsat: 0.000s)
CPU Time : 0.001s
```
The last output corresponds to modify the following function
```python
    def print_model(self, model: Model, default_printer: Callable[[], None]) -> None:
        if self._order.value:
            print(" ".join(str(sym) for sym in sorted(model.symbols(shown=True))))
        else:
            default_printer()
        sys.stdout.flush()
```