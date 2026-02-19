next(X) := #some{Y: edge(X,Y)} :- vertex(X).
start := #min{X: vertex(X)}.
visited(next(start)).
visited(next(X)) :- visited(X).
:- vertex(X), not visited(X).

% #showf next/1.

edge(a,b; a,d; b,c; b,e; c,a; d,f; e,d; f,c; f,e).
edge(X,Y) :- edge(Y,X).
vertex(X) :- edge(X,_).
vertex(X) :- edge(_,X).
