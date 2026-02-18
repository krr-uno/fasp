next(X) := #some{Y: edge(X,Y)} :- node(X).
start := #min{X: node(X)}.
visited(next(start)).
visited(next(X)) :- visited(X).
:- node(X), not visited(X).

#showf next/1.

edge(a,b; a,d; b,c; b,e; c,a; d,f; e,d; f,c; f,e).
vertex(X) :- edge(X,_).
vertex(X) :- edge(_,X).
