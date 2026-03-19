controlsStk(C1,C1,C2,P) :- ownsStk (C1,C2,P).
controlsStk(C1,C2,C3,P) :- company(C1), controls(C1,C2), ownsStk(C2,C3,P).
controls(C1,C3) :- company(C1), company(C3), #sum{P,C2 : controlsStk(C1,C2,C3,P)} > 50.

ownsStk(a,b,51).
ownsStk(a,c,30).
ownsStk(b,c,21).
ownsStk(c,b,21).

company(C):- ownsStk(C,_,_).
company(C):- ownsStk(_,C,_).

#show controls/2.
#show controlsStk/4.
