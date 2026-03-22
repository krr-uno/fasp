
edge(a,b) := 1. edge(b,c) := 2.
edge(a,c) := 5.
source := a. target := c.

reachable(source).
reachable(Y) :- in(X,Y), reachable(X).

{ in(X,Y) : edge(X,Y) = _ } = 1 :- reachable(X), X != target.
:- not reachable(target).

#minimize { edge(X,Y),X,Y : in(X,Y) }.

#show in/2.