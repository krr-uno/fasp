
edge(a,b,1). edge(b,c,2).
edge(a,c,5).
source(a). target(c).

reachable(X) :- source(X).
reachable(Y) :- in(X,Y), reachable(X).

{ in(X,Y) : edge(X,Y,_) } = 1 :- reachable(X), not target(X).

:- not reachable(X), target(X).

#minimize { W,X,Y : edge(X,Y,W), in(X,Y) }.

#show in/2.