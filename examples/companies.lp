controlsStk(C1,C1,C2) := ownsStk(C1,C2).
controlsStk(controller(C1),C1,C2) := ownsStk(C1,C2).
controller(C3) := C1 :- company(C1), company(C3), #sum{controlsStk(C1,C2,C3), C2} > 50.


ownsStk(a,b) := 51.
ownsStk(a,c) := 30.
ownsStk(b,c) := 21.
ownsStk(c,b) := 21.

company(C):- ownsStk(C,_) = _.
company(C):- ownsStk(_,C) = _.